
/* 
	Attach SCRSSN (and optionally, real SSN) to a supplied 
	list of patients (with ICN or SID as the identifier).
	Optionally, also get demographics from the SPatient
	table.  If <PTLIST> is not specified, you will get
	the whole cohort crosswalk +/- demographics from
	SPatient.

	J Smith
	02 Aug 2019

	Update 07 Sep 2021 -- added optional parameter SPt_cohort
	for subsetting SPatient_SPatient to a specific value of CohortName.

	Update 01 Dec 2021 -- changed the way the macro pulls raw
	data from SQL Server - now creates individual #temp tables
	reduced to only the required rows/columns, then joins
	afterward and drops temp tables when done. Old version
	has been moved to ./OLD

	Update 27 Nov 2023: added option useADRenroll to allow use of 
	ADR_ADREnrollHistory instead of Patient_Enrollment to get
	enroll start date and priority status.

	UPDATE 28 Jan 2025: fixed join issue with ADRenroll and 
	added option to use Src3 (Vista + Millenium) data.

	UPDATE Apr 2025: updated column name from Enrollmentdatetime
	to Enrollmentdate to match VINCI change.  

	======================================================== */

%MACRO getptx( /* INPUTS: <PTLIST> (list of pt ICNs) - optional */
	ptlist=, 
	joinvar=, 
	xwloc=Src,	/* location of CohortCrosswalk (Src or Dflt) */
	useADRenroll=0,	/* 1: use ADR_ADREnrollHistory to ascertain enrollment start date and priority status instead of Patient_Enrollment */
	useSrc3=0,	/* 1: use Src3 views instead of Src -- i.e., include Millenium EHR data -- note: this will force useADRenroll to 1 */
	overwrite=0,
	mknumSCRSSN=0, 
	realSSN=0, 
	getSta3n=0,
	sta3n_only1=1, /* set to 0 to keep all sta3n values */
	useSPt=0,
	SPt_cohort=,	/* if more than one cohort present in SPatient, specify which one here! This is NOT case-sensitive. */
	vetonly=1,
	remove_dups=1,	/* remove people if >1 SCRSSN per patientICN or >1 patientICN per SCRSSN */
	get_sector_update=1
	/* OUTPUTS: WORK.ptx */);

	%if &get_sector_update %then %do;
		%include '<path to common drive>/shapefiles/county/fromVA/FIPS_to_sector_BY22.sas';
	%end;


	%if &useSPt=0 and &getSta3n=1 %then %do;
		%put :: Sorry, you cannot get Sta3n without setting useSPt to 1!;
		%let getSta3n=0;
	%end;

	%let vwsuff=;
	%let Src=Src;
	%if &useSrc3 %then %do;
		%let vwsuff=_EHR;
		%let Src=Src3;
		%let useADRenroll=1;
	%end;

	%let hasptl=1;
	%if &ptlist= %then %do;
		%let hasptl=0;
		%let hasptSID=0;
		%let overwrite=0;
	%end;

	%if &hasptl %then %do;
		%if %index(&ptlist,.) %then %do;
			%let ptlib=%upcase(%scan(&ptlist,1,.));
			%let ptlist=%upcase(%scan(&ptlist,2,.));
		%end;
		%else %do;
			%let ptlib=WORK;
			%let ptlist=%upcase(&ptlist);
		%end;
		%if &joinvar= %then %do;
			proc sql undo_policy=NONE noprint;
			select distinct name into :ptidlist separated by ' '
			from dictionary.columns
			where upcase(memname)="&ptlist" and libname="&ptlib"
			and lowcase(name) in ('patienticn', 'patientsid', 'scrssn');
			quit;

			%if %sysfunc(countW(&ptidlist,' '))=1 %then %do;
				%let joinvar=%upcase(%sysfunc(compress(&ptidlist)));
			%end;
			%else %do;
				%put :: YOU MUST SUPPLY A VARIABLE TO JOIN ON (<joinvar>)!;
				%abort cancel;
			%end;
		%end;

		libname sdat sqlsvr datasrc=&PROJ &sql_optimal schema=dflt;

		data sdat."#&ptlist"n %if %lowcase(&joinvar)=patienticn %then (dbtype=(patientICN='varchar(50)'));;
		set &ptlib..&ptlist (keep=&joinvar);
		run;

		proc sql undo_policy=NONE noprint;
		select count(*) into :hasptSID from dictionary.columns
		where libname="&ptlib" and memname="&ptlist"
		and upcase(name)='PATIENTSID';	
		quit;
	%end;

	proc sql undo_policy=NONE noprint;
	connect to sqlsvr as sdat (datasrc=&PROJ &SQL_OPTIMAL);

	%if &Spt_cohort= %then %do;
		select compress("'" || CohortName || "'") into :coh_list separated by ','
		from connection to sdat (select distinct cohortName from &Src..SPatient_SPatient&vwsuff);
	%end;
	%else %do;
		select compress("'" || CohortName || "'") into :coh_list separated by ','
		from connection to sdat (select distinct cohortName from &Src..SPatient_SPatient&vwsuff
			where upper(cohortName)=upper("&SPt_cohort"));
	%end;
		
	%if &useSpt %then %do;
		%if &hasptl %then %do;
			* spatient - ptlist provided ;
			execute(select distinct sp.patientICN, sp.patientSID, sp.scrSSN, sp.sta3n, sp.veteranFlag,
				sp.birthdatetime, sp.deathdatetime, sp.gender into #spt
				from
					#&ptlist coh
					inner join
					(select patientICN, patientSID, scrSSN, sta3n, veteranFlag,
					birthdatetime, deathdatetime, gender from &Src..SPatient_SPatient&vwsuff 
					where CohortName in (&coh_list) %if &vetonly %then and VeteranFlag='Y';) sp
					on coh.&joinvar=sp.&joinvar) by sdat;
		%end;
		%else %do;
			* spatient - no ptlist provided ;
			execute(select distinct patientICN, patientSID, scrSSN, sta3n, veteranFlag, 
				birthdatetime, deathdatetime, gender into #spt
				from
					&Src..SPatient_SPatient&vwsuff where CohortName in (&coh_list) %if &vetonly %then and VeteranFlag='Y';) by sdat;
		%end;

		execute(select distinct patientSID, sta3n into #SIDlist from #spt) by sdat;

		* patient address ;
		execute(select distinct ad.patientSID, ad.gispatientaddresslatitude, ad.employmentstatus,
			ad.gispatientaddresslongitude, ad.gisfipscode, ad.gissector, ad.gisURH, ad.zip, ad.state 
			%if &getSta3n %then , ad.Sta3n; into #sad
			from
				#SIDlist sl
				inner join
				(select patientSID, gispatientaddresslatitude, gispatientaddresslongitude, employmentstatus,
				gisfipscode, gisSector, gisURH, zip, state, sta3n from &Src..SPatient_SPatientAddress&vwsuff 
				WHERE lower(addresstype)='patient' and lower(relationshiptopatient)='self'
				and CohortName in (&coh_list)) ad
				on sl.patientSID=ad.patientSID and sl.sta3n=ad.sta3n) by sdat;

		* patient enrollment ;
		* pull enrollment from Src.Patient_Enrollment regardless of whether also using ADR - this is 
		because ADR_ADREnrollHistory is missing enrollment date for about 1-2 percent of non-Millenium patients ;
		execute(select distinct en.patientSID, en.sta3n, en.enrollmentdate, en.enrollmentpriority into #sen
			from
				#SIDlist sl
				inner join
				(select patientSID, cast(enrollmentdatetime as date) as enrollmentdate, enrollmentpriority, sta3n
				from Src.Patient_Enrollment where CohortName in (&coh_list)) en
				on sl.patientSID=en.patientSID and sl.sta3n=en.sta3n) by sdat;

			%let ensrc=en;

		%if &useADRenroll %then %do;
			execute(select distinct a.ADRPersonICN as patientICN, 
				cast(en.enrollstartdate as date) as enrollmentdate, 
				case 
					when en.ADRPriorityGroupSID<1 then NULL 
					else cast(en.ADRpriorityGroupSID as varchar(50)) 
					end as enrollmentpriority into #senadr
				from
					[&PROJ].[Src].[Veteran_ADRPerson] A
					inner join
					(select ADRPersonSID, enrollStartDate,  /* this is actually a datetime */ 
					ADRPriorityGroupSID
					from [&PROJ].[Src].[ADR_ADREnrollHistory] where CohortName in (&coh_list)) EN
					on a.ADRPersonSID=en.ADRPersonSID;

				select a.*, b.enrollmentdate, b.enrollmentpriority into #sptsenadr
				from
					#spt A
					left join
					#senadr B
					on a.patientICN=b.patientICN;

				drop table if exists #senadr;

			) by sdat;
			
			%let ensrc=sp;
		
			execute(select a.*, b.enrollmentdate, b.enrollmentpriority into #sen2
					from 
						#spt A 
						inner join 
						#sen B
						on a.patientSID=b.patientSID and a.sta3n=b.sta3n;

				drop table if exists #sen;

				select * into #senall 
				from (
					select * from #sen2
					UNION
					select * from #sptsenadr) sub;

			) by sdat; 
		%end;

		/*
		title "# of recs in #senall";
		select nrecs from connection to sdat (select count(1) as nrecs from #senall);
		title "# of recs in #sptsenadr";
		select nrecs from connection to sdat (select count(1) as nrecs from #sptsenadr);
		create table chk1 as select * from connection to sdat (select top 10 * from #sptsenadr);
		title "# of recs in #sen2";
		select nrecs from connection to sdat (select count(1) as nrecs from #sen2);
		create table chk2 as select * from connection to sdat (select top 10 * from #sen2);
		title;
		disconnect from sdat;
		quit;

		title "chk1";
		proc print data=chk1 heading=v width=min; run;
		title "chk2";
		proc print data=chk2 heading=v width=min; run;
		title;

		endsas;
		proc sql; */

		execute(
			drop table if exists #sptsenadr;
			drop table if exists #sen2;
			) by sdat;

		* patient race ;
		execute(select distinct pr.patientSID, pr.sta3n, pr.race into #spr
			from
				#SIDlist sl
				inner join
				(select patientSID, sta3n, race from &Src..PatSub_PatientRace&vwsuff where CohortName in (&coh_list)) pr
				on sl.patientSID=pr.patientSID and sl.sta3n=pr.sta3n) by sdat;

		* patient ethnicity ;
		execute(select distinct pe.patientSID, pe.sta3n, pe.ethnicity into #spe
			from
				#SIDlist sl
				inner join
				(select patientSID, sta3n, ethnicity from &Src..PatSub_PatientEthnicity&vwsuff where CohortName in (&coh_list)) pe
				on sl.patientSID=pe.patientSID and sl.sta3n=pe.sta3n) by sdat;

	%end; *useSpt=1;

	create table WORK.ptx as 
	select DISTINCT patientICN length=10, 
	%if &realSSN %then patientSSN length=12 /* SSN field sometimes has a suffix */,; 
	scrSSN length=9
	%if &hasptSID %then , patientSID length=8;
	%if &getSta3n %then , sta3n length=4;
	%if &useSPt %then %do; 
		, VeteranFlag length=1, birthdatetime length=8, deathdatetime length=8, gender length=1,
		gispatientaddresslatitude as addr_lat length=8, gispatientaddresslongitude as addr_long length=8, 
		gisfipscode length=5, gisSector length=10, 
		%if &get_sector_update %then %do; put(gisfipscode,$Fmt_BY22_FIPS_Sector.) as gisSector_updt length=10, %end; 
		gisURH length=1, zip length=5, state length=20, /*country length=75,*/ 
		enrollmentdate length=5, enrollmentpriority length=8, race length=50, ethnicity length=30, employmentstatus as employment length=20
	%end;
	from connection to sdat (
		%if &useSPt %then %do;
			select sp.patientICN, sp.patientSID, sp.scrSSN, %if &realSSN %then sp.PatientSSN,; sp.VeteranFlag, 
			sp.birthdatetime, sp.deathdatetime, sp.gender, ad.gispatientaddresslatitude,ad.employmentstatus, 
			ad.gispatientaddresslongitude, ad.gisfipscode, ad.gisSector, ad.gisURH, ad.zip, ad.state,  %if &getSta3n %then ad.Sta3n, ; /*ad.country,*/
			&ensrc..enrollmentdate, &ensrc..enrollmentpriority, pr.race, pe.ethnicity
			from
				%if &useADRenroll %then #senall; %else #spt; sp
				left join
				#sad ad
				on sp.patientSID=ad.patientSID and sp.sta3n=ad.sta3n
				%if &useADRenroll=0 %then %do;
					left join
					#sen en
					on sp.patientSID=en.patientSID and sp.sta3n=en.sta3n
				%end;
				left join
				#spr pr
				on sp.patientSID=pr.patientSID and sp.sta3n=pr.sta3n
				left join
				#spe pe
				on sp.patientSID=pe.patientSID and sp.sta3n=pe.sta3n
		%end;
		%else %do;
			select distinct patientICN, scrSSN, patientSID from &xwloc..CohortCrosswalk
		%end;
		);

	%if &useSpt %then %do;
		%if &ptlist^= %then %do;
			execute(drop table #&ptlist) by sdat;
		%end;
		execute(drop table #spt) by sdat;
		execute(drop table #SIDlist) by sdat;
		execute(drop table #sad) by sdat;
		execute(drop table if exists #senall) by sdat;
		execute(drop table if exists #sen) by sdat;
		execute(drop table #spr) by sdat;
		execute(drop table #spe) by sdat;

		%put ::: NOTE: get.ptx used the following cohorts: &coh_list;
	%end;
	
	disconnect from sdat;

	quit;

	%let ssnlen=$9;

	%if &mknumSCRSSN %then %do;
		data WORK.ptx (rename=(scrssn2=scrssn));
		set WORK.ptx;
		length scrssn2 6;
		format scrssn2 ssn11.;
		scrssn2=scrssn*1;
		drop scrssn;
		run;

		%let ssnlen=6;
	%end;

	%if &getSta3n %then %do;
		* transpose horizontally if multiple ;
		proc sql undo_policy=NONE noprint;
		create table s3 as
		select distinct patientICN, sta3n
		from WORK.ptx
		order by patientICN;			/* <<-- would be better if we could sort the sta3ns longitudinally... */
		
		select max(n) into :maxs3 
		from (select patientICN, count(*) as n from s3 group by patientICN);
		quit;

		%let maxs3=%sysfunc(compress(&maxs3));

		data s3;
		set s3;
		by patientICN;
		length sta3n_1-sta3n_&maxs3 $4;
		retain sta3n_:;
		array s {*} sta3n_:;
		if first.patientICN then do;
			call missing(of s[*]);
			sn=0;
		end;
		sn+1;
		s[sn]=sta3n;
		if last.patientICN then output;
		keep patientICN sta3n_:;
		run;

		proc sort data=s3; by patientICN; run;	

		data WORK.ptx;
		set WORK.ptx;
		drop sta3n;
		run;
	%end;

	%if &useSPt %then %do;
		
		proc sort data=WORK.ptx; by patientICN enrollmentdate; run;

		data WORK.ptx (rename=(_race=race _ethnicity=ethnicity));
		set WORK.ptx;
		WHERE not missing(patientICN);
		by patientICN enrollmentdate;		
		length first_enroll 5 _race $50 _ethnicity $30 priority $1;
		format first_enroll date9.;
		retain first_enroll;
		if first.patientICN then do;
			first_enroll=.;
			_race='';
			_ethnicity='';
			priority='';
		end;
		if first_enroll=. then first_enroll=enrollmentdate;
		if NOT missing(race) then _race=race;
		if NOT missing(ethnicity) then _ethnicity=ethnicity;
		if NOT missing(enrollmentpriority) then 
			priority=substr(enrollmentpriority,anydigit(enrollmentpriority));
		if last.patientICN then OUTPUT;
		DROP enrollmentdate race ethnicity enrollmentpriority;
		run;

		data WORK.ptx (rename=(
			gender=sex _race=race gisfipscode=FIPS gisSector=Sector 
			%if &get_sector_update %then %do; gisSector_updt=Sector_updt %end; gisURH=URH));
		length 
			patientICN $10 %if &hasPtSID %then patientSID 8; scrSSN &SSNLEN 
			%if &realSSN %then PatientSSN $12; 
			veteranFlag $1 DOB 5 DOD 5 gender $1 _race $5 hispanic 3 race_orig $42 addr_lat addr_long 8 
			gisfipscode $5 gisSector $10 %if &get_sector_update %then %do; gisSector_updt $10 %end; 
			gisURH $1 zip $5 state $20 first_enroll 5 priority $1;
		set WORK.ptx;
		DOB=datepart(birthdatetime);
		DOD=datepart(deathdatetime);
		format DOB DOD first_enroll date9.;
		/*
		if ethnicity='HISPANIC OR LATINO' then _ethnicity='H';
		else if ethnicity='NOT HISPANIC OR LATINO' then _ethnicity='N';
		else if ethnicity='DECLINED TO ANSWER' then _ethnicity='d';
		else if ethnicity='UNKNOWN BY PATIENT' then _ethnicity='u'; */
		if ethnicity='HISPANIC OR LATINO' then hispanic=1;
		else if ethnicity='NOT HISPANIC OR LATINO' then hispanic=0;
		if index(race,'WHITE') then _race='W';
		/*else if race='BLACK OR AFRICAN AMERICAN' then _race='B';*/
		else if index(race,'BLACK') then _race='B';
		else if race='ASIAN' then _race='A';
		/*else if race='AMERICAN INDIAN OR ALASKA NATIVE' then _race='AI/AN';*/
		else if index(race,'AMERICAN IND') then _race='AI/AN';
		/*else if race='NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER' then _race='NH/PI';*/
		else if index(race,'HAWAII') then _race='NH/PI';
		/*else if race='DECLINED TO ANSWER' then _race='d';*/
		else if index(race,'DECLINE') then _race='d';
		else if upcase(race) IN: ('MISSING', 'UNKNOWN') then _race='u';
		race_orig=race;
		drop ethnicity race birthdatetime deathdatetime;
		run;

		%if &vetonly=0 %then %do;
			%put ::: IMPORTANT: the VetOnly flag was set to 0 - the output file may contain non-Veterans ;
		%end;
	%end;

	%if &hasptl %then %do;
		proc sql undo_policy=NONE noprint;		

		select compress('b.' || name) into :blist separated by ','
		from dictionary.columns where libname='WORK' and upcase(memname)='PTX'
		and upcase(name) NOT in 
			(select upcase(name) from dictionary.columns
			where libname="&ptlib" and upcase(memname)="&ptlist");

		create table 
			%if &overwrite %then 
				&ptlib..&ptlist; 
			%else 
				WORK.ptx; 
			as
		select a.* %if %symexist(blist) %then , &blist;
		from 
			&ptlib..&ptlist A
			LEFT join
			WORK.ptx B
			on a.&joinvar=b.&joinvar
		order by a.&joinvar;
		quit;
	%end;

	%if &getSta3n %then %do;
		data 
			%if &overwrite %then %do; 
				&ptlib..&ptlist
			%end;
			%else %do;
				WORK.ptx
			%end;
			%if &sta3n_only1 %then %do;
				(drop=sta3n_:)
			%end;
			;
		merge	
			%if &overwrite %then 
				&ptlib..&ptlist;
			%else
				WORK.ptx;
				(in=A)
			s3 (in=B)
			;
		by patientICN;  	/* <<-- note this will fail if <joinvar> is set to something other than patientICN - should fix */
		IF A;
		%if &sta3n_only1 %then %do;
			array s {*} sta3n:;
			length n_sta3n_values 3;
			n_sta3n_values=dim(s)-cmiss(of s[*]);
			sta3n_1=compress(sta3n_1);
			rename sta3n_1=sta3n; /* note that sta3n_1 is not necessarily the 'original' or most common sta3n! */
		%end;
		run;

		proc datasets lib=work memtype=data nolist nodetails;
		delete s3;
		run; quit;
	%end;

	%if &remove_dups %then %do;
		%let ptx=WORK.ptx;
		%if &overwrite %then %do;
			%let ptx=&ptlib..&ptlist;
		%end;

		* remove people whose SCRSSN is associated with >1 unique patientICN ;
		proc sort data=&ptx NODUPKEY; by scrssn patientICN; run;

		data &ptx;
		set &ptx end=last;
		by scrssn;
		retain nbad 0;
		if first.scrssn then do;
			if last.scrssn then output;
			else nbad+1;
		end;
		if last then put '::: # of people dropped d/t multiple patientICNs for a single SCRSSN: ' nbad;
		drop nbad;
		run;

		* remove people whose patientICN is associated with >1 unique SCRSSN ;
		proc sort data=&ptx NODUPKEY; by patientICN scrssn; run;

		data &ptx;
		set &ptx end=last;
		by patientICN;
		retain nbad 0;
		if first.patientICN then do;
			if last.patientICN then output;
			else nbad+1;
		end;
		if last then put '::: # of people dropped d/t multiple SCRSSNs for a single patientICN: ' nbad;
		drop nbad;
		run;
	%end;

%MEND; *getptx();


