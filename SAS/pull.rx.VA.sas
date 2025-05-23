/*
PROGRAM: pull.rx.VA.sas

Pull OP dispensations and IP administrations of drugs from CDW based 
on generic name and/or VA drug class.

vhawrjsmithj
NOTE: sample call for these two macros is at bottom

UPDATE Mar 2023 - all 'helper' macros which were previously defined in other
SAS programs have now been added to this program to make it stand-alone. These 
include %mklookup, %smush, %smush_search and %mkdrugfile.  The two macros
(also here) that need to be run independently by the user are getSIDs() and 
pull_drugs() as shown in the example call at the bottom.

UPDATE 30 Jan 2025: updated macro to allow use of Src3 (combined Vista + Millenium)
data.  To do this, set the new macro parameter, useSrc3, to 1.

================================================================================ */


%MACRO getSIDs(
	outlib=WORK,
	test_search=0,
	hasClassFile=1,
	hasNamesFile=1,
	exactNDC=0,	/* 1: join search_term to data based on exact NDC match, not LIKE (only relevant if hasNamesFile=1) */
	allow_partial_NDC=0,
	csvext=,	/* only applicable if test_search=1 */
	limit_smush_snippet=,
	useNatDrug=0,
	useSrc3=0		/* 0: use Src (Vista only) ... 1: use Src3 (Vista + Millenium) */
	);

	%global chkSrc3;
	%let chkSrc3=0;
	%if &useSrc3 %then %let chkSrc3=1;;

	%mklookup(valid_ndcs_only=0, inclNatDrug=&useNatDrug);

	title "output #1 -- from MKLOOKUP";
	proc print data=WORK.dimdrug (obs=5) heading=v width=min; run;

	%smush(drugclassfile=WORK.dimdrug, valid_ndcs_only=0, keeplocSID=1);

	%if %length(&limit_smush_snippet) %then %do;
		data work.base;
		set work.base;
		%&limit_smush_snippet
		run;
	%end;

	%if &test_search %then %do;
		%smush_search(inds=WORK.base, class_search=%if &hasClassFile %then class;, drug_search=%if &hasNamesFile %then names;, ext=&csvext);
		%put ::: drug search file was output in this directory as drug_search_results&csvext..csv ;
		%abort cancel;
	%end; 

	/*
	title "output #2 -- from SMUSH";
	proc print data=WORK.base (obs=5) heading=v width=min; run;

	data cov.class_drug_ndc (label=&SASPROG this is NOT a reliable list of NDCs!);
	set WORK.base;
	run; 
	*/


	title "first 10 obs of SMUSH output";
	proc print data=WORK.base (obs=10) heading=v width=min; run;

	%mkdrugfile(inds=WORK.base, class_search=%if &hasClassFile %then class;, drug_search=%if &hasNamesFile %then names;);

	title "first 10 obs of MKDRUGFILE output";
	proc print data=WORK.SIDS (obs=10) heading=v width=min; run;

	data &outlib..localdrugSIDs;
	set WORK.SIDs;
	run;

	/*
	first 10 obs of MKDRUGFILE output - this is all based on Dim tables, not patient data ;

												      L
		     s                                                                                o
		     e                                                                                c
		     a                                                                                a
		     r                                                  d                             l
		     c          c                                       r                             D
		     h          l                                       u                             r
		     _          a                 c                     g                             u
		     t          s                 l                     n                             g
	  O          e          s                 a                     a                n            S
	  b          r          I                 s                     m                d            I
	  s          m          D                 s                     e                c            D

	    1    %CORTICO%    HS050    ADRENAL CORTICOSTERIODS    betamethasone                     425282
	    2    %CORTICO%    HS050    ADRENAL CORTICOSTERIODS    betamethasone    00085-0566-00    462323
	    3    %CORTICO%    HS050    ADRENAL CORTICOSTERIODS    betamethasone    00085-0566-00    573989
	    4    %CORTICO%    HS050    ADRENAL CORTICOSTERIODS    betamethasone    00085-0566-00    711444
	    5    %CORTICO%    HS050    ADRENAL CORTICOSTERIODS    betamethasone    00085-0566-05     18287
	    6    %CORTICO%    HS050    ADRENAL CORTICOSTERIODS    betamethasone    00085-0566-05     25354
	    7    %CORTICO%    HS050    ADRENAL CORTICOSTERIODS    betamethasone    00085-0566-05     48834
	    8    %CORTICO%    HS050    ADRENAL CORTICOSTERIODS    betamethasone    00085-0566-05     57927
	    9    %CORTICO%    HS050    ADRENAL CORTICOSTERIODS    betamethasone    00085-0566-05     59859
	   10    %CORTICO%    HS050    ADRENAL CORTICOSTERIODS    betamethasone    00085-0566-05    102317 */

%MEND; *getSIDS();

* NOTE: the mkloopup macro defined below is called by the getSIDs macro above - it does not need to be run separately ;
%MACRO mklookup(valid_ndcs_only=1, inclNatDrug=1);

	%let CWnum=;
	%let vwsuff=;
	%if &chksrc3 %then %do;
		%let vwsuff=_EHR;
		%let CWnum=3;
	%end;

	proc sql undo_policy=NONE;
	connect to sqlsvr as sdat (datasrc=&PROJ &SQL_OPTIMAL);

	create table WORK.dimdrug as select * from connection to sdat
		(
		select * from CDWWork&CWnum..Dim.DrugClass&vwsuff;
		); * by sdat;

	create table WORK.dimlocdrug as select *, 'L' as sidsource from connection to sdat
		(
			select a.VAclassification, a.drugclassSID, a.sta3n, b.drugnamewithoutdose, a.localdrugSID, a.ndc
			from
				CDWWork&CWnum..Dim.LocalDrug&vwsuff A
				inner join
				CDWWork&CWnum..Dim.DrugNameWithoutDose&vwsuff B
				on a.drugnamewithoutdoseSID=b.drugnamewithoutdoseSID and a.sta3n=b.sta3n;
		); * by sdat;

	create table WORK.dimnatdrug as select *, ' ' as ndc length=50, 'N' as sidsource length=1 
	from connection to sdat
		(
			select c.DrugClassCode as VAclassification, a.primarydrugclassSID as drugclassSID, a.sta3n,
			b.drugnamewithoutdose, a.nationaldrugSID as localdrugSID
			from
				CDWWork&CWnum..Dim.NationalDrug&vwsuff A
				inner join
				CDWWork&CWnum..Dim.DrugNameWithoutDose&vwsuff B
				on a.drugnamewithoutdoseSID=b.drugnamewithoutdoseSID and a.sta3n=b.sta3n
				inner join
				CDWWork&CWnum..Dim.DrugClass&vwsuff C
				on a.PrimaryDrugClassSID=c.drugclassSID and a.sta3n=c.sta3n
		); * by sdat;

	disconnect from sdat;
	quit;

	
	%if &inclNatDrug %then %do;
		proc sql undo_policy=NONE;
		create table WORK.dimlocdrug as
		select * from WORK.dimlocdrug
		UNION ALL
		select * from WORK.dimnatdrug;

		title "distinct drugs found in local vs. national - includes dups";
		select sidsource, count(distinct localdrugsid) as n_distinct_drugSIDs, 
		count(distinct drugnamewithoutdose) as n_distinct_drugnames
		from WORK.dimlocdrug 
		group by sidsource
		order by sidsource;
		title;

		drop table WORK.dimnatdrug;
		quit;
	%end;

	data WORK.dimlocdrug;
	length drugclass $10 drugnamewithoutdose $100 ndc $13;
	set WORK.dimlocdrug (keep=VAclassification /* edit 08 Feb 2022 - VINCI removed column drugclass... drugclass*/ 
		drugclassSID sta3n drugnamewithoutdose localdrugSID ndc sidsource RENAME=(VAclassification=DRUGCLASS));
	length weird_ndc $1;
	if missing(ndc) then do;
		weird_ndc='m';
	end;
	else if substr(ndc,1,1)='S' then do;
		weird_ndc='s';
		ndc=substr(ndc,2);
	end;
	if countc(ndc,'-')^=2 then do;
		weird_ndc='?';
	end;
	else do;
		if missing(weird_ndc) then do;
			weird_ndc='-';
			if length(ndc)>13 then weird_ndc='L';
		end;
		ndc=compress(put(scan(ndc,1,'-')*1,z5.) || '-' || put(scan(ndc,2,'-')*1,z4.) || '-' || put(scan(ndc,3,'-')*1,z2.));
		if index(ndc,'.') then weird_ndc='.';
	end;
	run;

	proc freq data=WORK.dimlocdrug;
	table weird_ndc/missing;
	run;

	data WORK.dimlocdrug;
	set WORK.dimlocdrug;
	%if &valid_ndcs_only %then %do;
		IF weird_ndc in ('s','-');
		drop weird_ndc;
	%end;
	run;

%MEND; *mklookup;

* NOTE: the smush() macro defined below is called by the getSIDs macro at the top of this program - it does not need to be run separately ;
%macro smush(drugclassfile=, valid_ndcs_only=1, keeplocSID=0);

	%let more_drugs=1;
	%let N=0;

	data WORK.base (rename=(drugclassSID=dcs&N sta3n=_sta3n&N drugclasscode=dcc&N drugclassification=class&N));
	length drugclassSID 8 drugclasscode $5 drugclassification $50; * this will definitely truncate! ;
	set &drugclassfile (keep=drugclassSID sta3n drugclasscode drugclassification parentdrugclassSID);
	WHERE parentdrugclassSID=-1 and drugclassSID>0;
	if lowcase(drugclasscode)='*unkn' THEN DELETE;
	drop parentdrugclassSID;
	run;

	%let orderstr=dcs&N;
	%let orderstr2=dcc&N;

	proc sort data=WORK.base NODUPKEY; by dcs&N; run;

	proc sql undo_policy=NONE noprint;
	select count(distinct dcc&N) into :totdrugs from WORK.base;
	quit;

	%put :: COUNTDCC: &totdrugs;
	
	%do %while (&more_drugs);

		%let Nlast=&N;
		%let N=%eval(&N+1);

		proc sql undo_policy=NONE noprint;
		create table WORK.base as
		select distinct a.*, b.drugclassSID as dcs&N, b.sta3n as _sta3n&N, 
		b.drugclasscode as dcc&N length=5, drugclassification as class&N length=50
		from
			WORK.base A
			left join
			&drugclassfile B
			on a.dcs&Nlast=b.parentdrugclassSID and a._sta3n&Nlast=b.sta3n
		order by &orderstr;
		
		select count(distinct dcc&N) into :countdcc from WORK.base
		where not missing(dcc&N);
		quit;

		%put :: COUNTDCC: &countdcc;

		%if &countdcc=0 %then %do;
			%let more_drugs=0;
			data WORK.base;
			set WORK.base;
			drop dcs&N dcc&N class&N;
			run;
		%end;
		%else %do;
			%let orderstr=&orderstr, dcs&N;
			%let orderstr2=&orderstr2, dcc&N;
		%end;

		%let totdrugs=%eval(&totdrugs+&countdcc);
	
	%end;

	data WORK.base;
	set WORK.base;
	drop dcs: _sta3n:;
	run;

	proc sort data=WORK.base NODUP; by %do i=1 %to &Nlast; dcc&i %end;; run;

	proc sql undo_policy=NONE;
	create table WORK.base as
	select DISTINCT a.*, b.drugclass as ldclass, . as classnum length=3, 
	lowcase(b.drugnamewithoutdose) as drugname, b.ndc %if &keeplocSID %then , b.localdrugSID, b.sta3n, b.sidsource;
	from 
		WORK.base A
		INNER join
		(select * from WORK.dimlocdrug 
		where lowcase(drugnamewithoutdose)^='*missing*' %if &valid_ndcs_only %then and anyalpha(ndc)=0 and ndc^='99999-9999-99';) B
		on
			a.dcc0=b.drugclass
			%do i=1 %to &Nlast;
				or 
				a.dcc&i=b.drugclass
			%end;
	order by &orderstr2;
	quit;
	
	data WORK.base;
	set WORK.base;
	array c {*} dcc:;
	do i=1 to dim(c);
		if c[i]=ldclass then do;
			classnum=i-1;
		end;
	end;
	drop i ldclass;
	run;

	/*
	proc print data=WORK.base (obs=200) heading=vertical width=min;
	run; */

	%put :: TOTAL UNIQUE DRUG CLASSES: &totdrugs;

%mend; *smush();

* NOTE: the smush_search() macro defined below is called by the getSIDs macro at the top of this program - it does not need to be run separately ;
%macro smush_search(inds=, class_search=, drug_search=, ext=);

	%if &class_search^= %then %do;
		proc sql undo_policy=NONE;
		create table search_by_class as
		select distinct 'CL' as search_type length=2, b.search_term,
		a.dcc0, a.class0, a.dcc1, a.class1, a.dcc2, a.class2, a.classnum, a.drugname 
		from 
			&inds A
			inner join
			&class_search B
			on 
				upcase(a.dcc0) LIKE trim(upcase(b.search_term)) OR upcase(a.class0) LIKE trim(upcase(b.search_term))
				OR
				upcase(a.dcc1) LIKE trim(upcase(b.search_term)) OR upcase(a.class1) LIKE trim(upcase(b.search_term))
				OR
				upcase(a.dcc2) LIKE trim(upcase(b.search_term)) OR upcase(a.class2) LIKE trim(upcase(b.search_term))
		order by b.search_term, a.dcc0, a.dcc1, a.dcc2;
	%end;
	%if &drug_search^= %then %do;
		proc sql undo_policy=NONE;
		create table search_by_drugname as
		select distinct 'NM' as search_type length=2, b.search_term,
		a.dcc0, a.class0, a.dcc1, a.class1, a.dcc2, a.class2, a.classnum, a.drugname
		from
			&inds A
			inner join
			&drug_search B
			%if &exactNDC %then %do;
				on a.ndc=b.search_term
			%end;
			%else %do;
				on upcase(a.drugname) LIKE trim(upcase(b.search_term)) 
					%if &allow_partial_NDC %then %do;
						OR upcase(a.ndc) LIKE trim(upcase(b.search_term))
					%end;
			%end;
		order by b.search_term, a.dcc0, a.dcc1, a.dcc2;
	%end;
	
	data search_results;
	set
		search_by_:
		;
	run;

	data _nocommas;
	set search_results;
	array c {*} class0 class1 class2 drugname;
	do i=1 to dim(c);
		c[i]=tranwrd(c[i],',','');
	end;
	drop i;
	run;

	title "first 50 search results based on input files: &class_search &drug_search";
	proc print data=search_results (obs=50) heading=v width=min; run;
	title;

	%if %symexist(CWD) %then %do;
		proc export data=_nocommas dbms=csv replace
			outfile="&CWD/drug_search_results&ext..csv";
		run;
	%end;

%mend; *smush_search();

* NOTE: the mkdrugfile() macro defined below is called by the getSIDs macro at the top of this program - it does not need to be run separately ;
%macro mkdrugfile(inds=, class_search=, drug_search=);

	%if &class_search^= %then %do;
		proc sql undo_policy=NONE;
		create table SIDs_from_class as
		select distinct b.search_term, a.dcc0 as classID length=5, a.class0 as class length=50, a.drugname, a.ndc, a.localdrugSID, a.sta3n, a.sidsource
		from 
			&inds A
			inner join
			&class_search B
			on upcase(a.dcc0) LIKE trim(upcase(b.search_term)) OR upcase(a.class0) LIKE trim(upcase(b.search_term))
		UNION
		select distinct b.search_term, a.dcc1 as classID length=5, a.class1 as class length=50, a.drugname, a.ndc, a.localdrugSID, a.sta3n, a.sidsource
		from 
			&inds A
			inner join
			&class_search B
			on upcase(a.dcc1) LIKE trim(upcase(b.search_term)) OR upcase(a.class1) LIKE trim(upcase(b.search_term))
		UNION
		select distinct b.search_term, a.dcc2 as classID length=5, a.class2 as class length=50, a.drugname, a.ndc, a.localdrugSID, a.sta3n, a.sidsource
		from 
			&inds A
			inner join
			&class_search B
			on upcase(a.dcc2) LIKE trim(upcase(b.search_term)) OR upcase(a.class2) LIKE trim(upcase(b.search_term))
		order by search_term, classID, class, drugname, ndc, localdrugSID;
		quit;
	%end;
	%if &drug_search^= %then %do;
		proc sql undo_policy=NONE;
		create table SIDs_from_drugname as
		select distinct b.search_term, coalesceC(a.dcc2, a.dcc1, a.dcc0) as classID length=5,  /* note this takes the lowest-level class available */
		coalesceC(a.class2, a.class1, a.class0) as class length=50, a.drugname, a.ndc, a.localdrugSID, a.sta3n, a.sidsource
		from
			&inds A
			inner join
			&drug_search B
			%if &exactNDC %then %do;
				on a.ndc=b.search_term
			%end;
			%else %do;
				on upcase(a.drugname) LIKE trim(upcase(b.search_term)) 
					%if &allow_partial_NDC %then %do;
						OR upcase(a.ndc) LIKE trim(upcase(b.search_term))
					%end;
			%end;
		order by search_term, classID, class, drugname, ndc, localdrugSID;
	%end;

	data SIDs;
	set 
		SIDs_from_:
		;
	run;

%mend; *mkdrugfile();


* NOTE: this macro pulls drugs from CDW based on the list of SIDs generated by the getSIDs() macro at the top of this program ;
%MACRO pull_drugs(
	rxppl=,	/* list of patientICNs in a SAS dataset - if omitted, defaults to entire CohortCrosswalk */
	sidsFile=, /* SAS dataset of drug SIDs and associated classifiers of interest such as that produced by %getSIDs */
	startdate=,	/* global start date for drug pull -- not patient-specific */
	enddate=,	/* global end date for drug pull -- not patient-specific */
	CC=Src  /* CC: Src or Dflt -- location of CohortCrosswalk */
	);

	%if %symexist(chkSrc3)=0 %then %do;
		%put ::: ERROR: to use the pull_drugs macro, you must either run the getSIDs macro first (in this program) ;
		%put ::: ...or create a comparable SIDs file and also create the macro variable chkSrc3 (set to 0 or 1) ;
		%put ::: SAS is quitting ;
		%abort cancel;
	%end;

	%let CWnum=;
	%let vwsuff=;
	%if &chksrc3 %then %do;
		%let vwsuff=_EHR;
		%let CWnum=3;
	%end;

	%if &rxppl^= %then %do;
		data sdat.'#rxppl'n (dbtype=(patientICN="varchar(50)"));
		set &rxppl (keep=patientICN);
		run;
	%end;

	data 
		WORK.SIDs
		WORK.nSIDs /* these are actually national drug SIDs d/t weirdness of CDW for some drugs */
		;
	set &sidsfile;
	if sidsource='L' then output WORK.SIDs;
	else if sidsource='N' then output WORK.nSIDs;
	run;

	%let nnsids=0;

	proc sql undo_policy=NONE noprint;
	select count(*) into :nnsids from &sidsfile WHERE sidsource='N';
	quit;

	proc sort data=WORK.SIDs NODUPKEY;
	by localdrugSID sta3n;
	run;

	* transfer the drug SIDs for drugs of interest to SQL Server... ;
	data sdat.'#LDsids'n (dbtype=(search_term='varchar(50)' class='varchar(50)' 
		drugname='varchar(100)' ndc='varchar(13)' localdrugSID='bigint'));
	set WORK.SIDs (keep=search_term class drugname ndc localdrugSID sta3n);
	run;

	%if &nnsids %then %do;
		proc sort data=WORK.nSIDs NODUPKEY;
		by localdrugSID sta3n;
		run;
		
		data sdat.'#NDsids'n (dbtype=(search_term='varchar(50)' class='varchar(50)' 
			drugname='varchar(100)' ndc='varchar(13)' nationaldrugSID='bigint'));
		set WORK.nSIDs (keep=search_term class drugname ndc localdrugSID rename=(localdrugSID=nationaldrugSID));
		run;
	%end;

	proc sql undo_policy=NONE;
	connect to sqlsvr as sdat (datasrc=&PROJ &SQL_OPTIMAL);

	%if &rxppl= %then %do;
		execute(select distinct patientICN into #rxppl from [&PROJ].[&CC].[CohortCrosswalk]) by sdat;
	%end;

	create table WORK.rxtemp as 
	select DISTINCT patientICN length=10, patientsid length=8, rxEventSID length=8, parentSID length=8,
	localdrugsid length=8, sta3n length=4, /*datepart(rxdatetime) as rxdate length=5 format=date9.*/ rxdatetime length=7 format=datetime20.,  
	dayssupply length=5, qtynumeric length=8, qtychar length=10, unit length=20, infusionrate length=20, mailwindow length=1,
	search_term length=20, class length=50, drugname length=75, ndc length=13, source length=4
	from connection to sdat

		(
		select ptx.patientICN, ptx.patientSID, 
		drugs.rxoutpatfillSID as rxEventSID, drugs.rxoutpatSID as parentSID, drugs.sta3n, drugs.localdrugSID,
		drugs.dayssupply, drugs.filldatetime as rxdatetime, 
		drugs.qtynumeric, NULL as qtychar, NULL as unit, NULL as infusionrate,
		drugs.mailwindow, drugs.search_term, drugs.class, drugs.drugname, drugs.ndc, 'OPFL' as source
		from
			(select xw.patientICN, xw.patientSID, xw.sta3n
			from
				#rxppl CP
				inner join
				[&PROJ].[&CC].[cohortcrosswalk] xw
				on cp.patientICN=xw.patientICN) ptx
		inner join

		(select distinct rx.patientsid, rx.rxoutpatfillSID, rx.rxoutpatSID, rx.sta3n, rx.localdrugsid,
		rx.filldatetime, rx.dayssupply, rx.qtynumeric, rx.mailwindow,
		sids.search_term, sids.class, sids.drugname, sids.ndc
		from 
			#LDsids sids
			inner join 
			(select patientsid, rxoutpatfillSID, rxoutpatSID, sta3n, localdrugsid, filldatetime, 
			dayssupply, qtynumeric, mailwindow
			from [&PROJ].[Src&CWnum].[RxOut_RxOutpatFill&vwsuff] 
			where 
				filldatetime >= cast(%nrbquote(')&startdate.T00:00:00.000%nrbquote(') as datetime2(0))
				and
				filldatetime < cast(%nrbquote(')&enddate.T00:00:00.000%nrbquote(') as datetime2(0))) rx
			on sids.localdrugsid=rx.localdrugsid and sids.sta3n=rx.sta3n) drugs
		on ptx.patientsid=drugs.patientsid and ptx.sta3n=drugs.sta3n

		%if &nnsids %then %do;
			UNION ALL

			select ptx.patientICN, ptx.patientSID, 
			drugs.rxoutpatfillSID as rxEventSID, drugs.rxoutpatSID as parentSID, drugs.sta3n, drugs.localdrugSID, 
			drugs.dayssupply, drugs.filldatetime as rxdatetime, 
			drugs.qtynumeric, NULL as qtychar, NULL as unit, NULL as infusionrate,
			drugs.mailwindow, drugs.search_term, drugs.class, drugs.drugname, drugs.ndc, 'nOFL' as source
			from
				(select xw.patientICN, xw.patientSID, xw.sta3n
				from
					#rxppl CP
					inner join
					[&PROJ].[&CC].[cohortcrosswalk] xw
					on cp.patientICN=xw.patientICN) ptx
			inner join

			(select distinct rx.patientsid, rx.rxoutpatfillSID, rx.rxoutpatSID, rx.sta3n, rx.localdrugsid,
			rx.filldatetime, rx.dayssupply, rx.qtynumeric, rx.mailwindow,
			sids.search_term, sids.class, sids.drugname, sids.ndc
			from 
				#NDsids sids
				inner join 
				(select patientsid, rxoutpatfillSID, rxoutpatSID, sta3n, localdrugsid, nationaldrugsid, filldatetime, 
				dayssupply, qtynumeric, mailwindow
				from [&PROJ].[Src&CWnum].[RxOut_RxOutpatFill&vwsuff] 
				where 
					filldatetime >= cast(%nrbquote(')&startdate.T00:00:00.000%nrbquote(') as datetime2(0))
					and
					filldatetime < cast(%nrbquote(')&enddate.T00:00:00.000%nrbquote(') as datetime2(0))) rx
				on sids.NATIONALdrugsid=rx.NATIONALdrugsid) drugs
			on ptx.patientsid=drugs.patientsid and ptx.sta3n=drugs.sta3n
		%end;

		UNION ALL

		select ptx.patientICN, ptx.patientSID, 
		drugs.BCMADispensedDrugSID as rxEventSID, drugs.BCMAMedicationLogSID as parentSID, drugs.sta3n,
		drugs.localdrugSID, NULL as dayssupply, drugs.actiondatetime as rxdatetime,
		drugs.dosesgiven as qtynumeric, NULL as qtychar, drugs.unitofadministration as unit, drugs.infusionrate,
		'-' as mailwindow, drugs.search_term, drugs.class, drugs.drugname, drugs.ndc, 'BCDI' as source
		from
			(select xw.patientICN, xw.patientSID, xw.sta3n
			from
				#rxppl CP
				inner join
				[&PROJ].[&CC].[cohortcrosswalk] xw
				on cp.patientICN=xw.patientICN) ptx
		inner join
		
		(select distinct rx.patientsid, rx.Sta3n, rx.BCMADispensedDrugSID, rx.BCMAMedicationLogSID, 
		rx.localdrugsid, rx.actiondatetime, rx.dosesgiven, rx.unitofadministration, rx.infusionrate,
		sids.search_term, sids.class, sids.drugname, sids.ndc
		from 
			#LDsids sids
			inner join 
			(select bd.*, ml.patientSID, ml.infusionrate
			from
				(select sta3n, BCMADispensedDrugSID, BCMAMedicationLogSID, 
				localdrugSID, actiondatetime, 
				dosesgiven, 			/* <<-- the Dispensed Drug table has a NUMERIC type column named doseSgiven */
				unitofadministration   
				from [&PROJ].[Src&CWnum].[BCMA_BCMADispensedDrug&vwsuff]
				where
					actiondatetime >= cast(%nrbquote(')&startdate.T00:00:00.000%nrbquote(') as datetime2(0))
					and
					actiondatetime < cast(%nrbquote(')&enddate.T00:00:00.000%nrbquote(') as datetime2(0))) bd
				inner join
				[&PROJ].[Src&CWnum].[BCMA_BCMAMedicationLog&vwsuff] ml
				on bd.BCMAmedicationlogsid=ml.BCMAmedicationlogsid and bd.sta3n=ml.sta3n) rx

			on sids.localdrugsid=rx.localdrugsid and sids.sta3n=rx.sta3n) drugs
		on ptx.patientSID=drugs.patientSID and ptx.sta3n=drugs.sta3n

		UNION ALL

		select ptx.patientICN, ptx.patientSID, 
		drugs.BCMASolutionSID as rxEventSID, drugs.BCMAMedicationLogSID as parentSID, drugs.sta3n,
		drugs.localdrugSID, NULL as dayssupply, drugs.actiondatetime as rxdatetime,
		NULL as qtynumeric, drugs.dosesgiven as qtychar, drugs.unitofadministration as unit, drugs.infusionrate,
		'-' as mailwindow, drugs.search_term, drugs.class, drugs.drugname, drugs.ndc, 'BCSO' as source
		from
			(select xw.patientICN, xw.patientSID, xw.sta3n
			from
				#rxppl CP
				inner join
				[&PROJ].[&CC].[cohortcrosswalk] xw
				on cp.patientICN=xw.patientICN) ptx
		inner join
		
		(select distinct rx.patientsid, rx.Sta3n, rx.BCMASolutionSID, rx.BCMAMedicationLogSID, 
		rx.localdrugsid, rx.actiondatetime, rx.dosesgiven, rx.unitofadministration, rx.infusionrate,
		sids.search_term, sids.class, sids.drugname, sids.ndc
		from 
			#LDsids sids
			inner join 
			(select bs.*, ivs.localdrugSID, ml.patientSID, ml.infusionrate 
			from
				(select sta3n, BCMASolutionSID, BCMAMedicationLogSID, 
				IVSolutionIngredientSID, actiondatetime, 
				dosesgiven, 		/* <<-- the Solution table has a character type column named doseSgiven */
				unitofadministration
				from [&PROJ].[Src&CWnum].[BCMA_BCMASolution&vwsuff]
				where
					actiondatetime >= cast(%nrbquote(')&startdate.T00:00:00.000%nrbquote(') as datetime2(0))
					and
					actiondatetime < cast(%nrbquote(')&enddate.T00:00:00.000%nrbquote(') as datetime2(0))) bs
				inner join
				[CDWWork&CWnum].[Dim].[IVSolutionIngredient&vwsuff] ivs
				on bs.ivsolutionIngredientSID=ivs.ivsolutionIngredientSID and bs.sta3n=ivs.sta3n
				inner join
				[&PROJ].[Src&CWnum].[BCMA_BCMAMedicationLog&vwsuff] ml
				on bs.BCMAmedicationlogsid=ml.BCMAmedicationlogsid and bs.sta3n=ml.sta3n
				) rx
			on sids.localdrugSID=rx.localdrugSID and sids.sta3n=rx.sta3n) drugs
		on ptx.patientSID=drugs.patientSID and ptx.sta3n=drugs.sta3n

		UNION ALL

		select ptx.patientICN, ptx.patientSID, 
		drugs.BCMAAdditiveSID as rxEventSID, drugs.BCMAMedicationLogSID as parentSID, drugs.sta3n,
		drugs.localdrugSID, NULL as dayssupply, drugs.actiondatetime as rxdatetime,
		NULL as qtynumeric, drugs.dosegiven as qtychar, drugs.unitofadministration as unit, drugs.infusionrate,
		'-' as mailwindow, drugs.search_term, drugs.class, drugs.drugname, drugs.ndc, 'BCAD' as source
		from
			(select xw.patientICN, xw.patientSID, xw.sta3n
			from
				#rxppl CP
				inner join
				[&PROJ].[&CC].[cohortcrosswalk] xw
				on cp.patientICN=xw.patientICN) ptx
		inner join
		
		(select distinct rx.patientsid, rx.Sta3n, rx.BCMAAdditiveSID, rx.BCMAMedicationLogSID, 
		rx.localdrugsid, rx.actiondatetime, rx.dosegiven, rx.unitofadministration, rx.infusionrate,
		sids.search_term, sids.class, sids.drugname, sids.ndc
		from 
			#LDsids sids
			inner join 
			(select ba.*, iva.localdrugSID, ml.patientSID, ml.infusionrate 
			from
				(select sta3n, BCMAAdditiveSID, BCMAMedicationLogSID, 
				IVAdditiveIngredientSID, actiondatetime, 
				dosegiven, 	/* <<-- the additive table has a character type column named dosegiven (not doseSgiven) */
				unitofadministration 
				from [&PROJ].[Src&CWnum].[BCMA_BCMAAdditive&vwsuff]
				where
					actiondatetime >= cast(%nrbquote(')&startdate.T00:00:00.000%nrbquote(') as datetime2(0))
					and
					actiondatetime < cast(%nrbquote(')&enddate.T00:00:00.000%nrbquote(') as datetime2(0))) ba
				inner join
				[CDWWork&CWnum].[Dim].[IVAdditiveIngredient&vwsuff] iva
				on ba.IVAdditiveIngredientSID=iva.IVAdditiveIngredientSID and ba.sta3n=iva.sta3n
				inner join
				[&PROJ].[Src&CWnum].[BCMA_BCMAMedicationLog&vwsuff] ml
				on ba.BCMAmedicationlogsid=ml.BCMAmedicationlogsid and ba.sta3n=ml.sta3n
				) rx
			on sids.localdrugSID=rx.localdrugSID and sids.sta3n=rx.sta3n) drugs
		on ptx.patientSID=drugs.patientSID and ptx.sta3n=drugs.sta3n


		);

	execute(drop table if exists #rxppl) by sdat;
	execute(drop table if exists #LDsids) by sdat;

	disconnect from sdat;

	quit;

%MEND; *pull_drugs();



/*
* SAMPLE INPUT AND CALL FOR %getSIDs... ;


%let PROJ=ORD_Korves_202209016D;  * ORD project ;
%let CC=Src; * location of CohortCrosswalk -- either Src or Dflt ;

libname sdat sqlsvr datasrc=&PROJ &sql_optimal schema=dflt;


data 
	class (keep=search_term)
	classfmt (rename=(search_term=start))
	;
infile cards dlm=',';
length search_term $50 label $15;
input search_term label;
cards;
%VIRAL%,antiviral
%MALAR%,antimalar
%NSAI%,nsaid
%CORTICO%,corticosteroid
%ANTI%COAG%,anticoag
%PLATELET%,pai
%ANGIOTENSIN%,acearb
%ACE%INHIB%,acearb
;
run;

data 
	names (keep=search_term)
	namesfmt (rename=(search_term=start))
	;
infile cards dlm=',';
length search_term $50 label $15;
input search_term label;
cards;
%acetamin%,nsaid
%aspirin%,nsaid
%lisinopril%,acearb
%enalopril%,acearb
%captopril%,acearb
%benazepril%,acearb
%fosinopril%,acearb
%moexipril%,acearb
%perindopril%,acearb
%quinapril%,acearb
%ramopril%,acearb
%trandolapril%,acearb
%fluvoxamine%,adepr
;
run;

data rxfmt;
set
	classfmt
	namesfmt
	;
length fmtname $8 type $1 end $15;
fmtname='$frxcat';
type='C';
end=start;
run;

data rxfmt;
set rxfmt end=last;
output;
if last then do;
	hlo='O';
	label='junk';
	output;
end;
run;

proc format cntlin=rxfmt; run;  */

/*

%let sidlib=WORK;
%let startdate=2020-01-01;
%let enddate=2021-12-31;

%getSIDs(outlib=&sidlib);  * produces WORK.localdrugSIDs ; 

* create a population to pull drugs for... ;
data poslabs; 
set surnew.covid_labs_clean (keep=patientICN resclean specdatetime);
where resclean='P';
length specdate 5;
format specdate date9.;
specdate=datepart(specdatetime);
drop specdatetime;
IF '01Jan2020'd<=specdate<='31Dec2021'd;
run;

data covdx;
set surnew.covid_dx_va20202021 (keep=patientICN admit_or_vis_datetime factype code);
where code='U07.1';
length dxdate 5;
format dxdate date9.;
dxdate=datepart(admit_or_vis_datetime);
drop admit_or_vis_datetime;
IF '01Jan2020'd<=dxdate<='31Dec2021'd;
run;

data covppl;
set
	poslabs
	covdx
	;
keep patientICN;
run;

proc sort data=covppl NODUPKEY; by patientICN; run;

* pull drugs of interest for population ;
%pull_drugs(
	rxppl=covppl, 
	sidsFile=WORK.localdrugSIDs,
	startdate=&startdate,
	enddate=&enddate,
	CC=Dflt);  


SAMPLE PROCESSING OF OUTPUT FILE WORK.rxtemp (from %pull_drugs())...

data req.varx;
set WORK.rxtemp;
if source in ('BCSO', 'BCAD') then do;
	qtynumeric=scan(qtychar,1,' ')*1;
	unit=coalesceC(unit, scan(qtychar,2,' '));
end;
drop qtychar;  * <<-- might want to hang on to this and take a look ;
run;

title "SEARCH TERM * DATA SOURCE";
proc freq data=req.varx;
table search_term * source/missing;
run;  */

