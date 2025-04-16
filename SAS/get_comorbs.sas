/*
	Take a user-supplied list of patients with indexdates (can be >1 indexdate per person) and a user-supplied
	list of disease definitions and link these to inpatient and outpatient encounter diagnosis data in CDW.  

	Create a permanent table of the above query on the SQL Server side (and, optionally, a view to that table 
	on the SAS Grid side) and also allow collapsing all diseases to the patient level using standard 1 inpt / 2 outpt 
	method.  

	Jeremy Smith
	Jan 2018

	NOTES: if the option dropSQLtable=1 is selected the macro will automatically set the option keepOPfails to 1

	UPDATE 02 Aug 2022: added 'POAindicator' field (Y/N indicator for whether a given dx was present-on-admission) to
		event-level output -- in order to get this field, user must set the IPDX parameter to use 
		Src.Inpat_InpatientDiagnosis (as opposed to Src.Inpat_InpatDischargeDiagnosis).

	UPDATE 29 Jan 2025: added capacity to use Src3 (combined Vista + Millenium) data instead of just Vista-based data.
		To do this, specify the relevant Src3 SQL views (all of which will have the suffix '_EHR') in the 
		ipstays, ipdx, opstays, and opdx parameters.  Doing so will cause the macro to automatically use the
		corresponding Dim tables.

	================================================================================================================ */

%MACRO pull_dxpx(
	ptfile=,		/* should contain: patientICN ($10), indexdate (date or datetime) */
	codefile=,		/* should contain: code ($10), codetype ($6), condition ($32) 
					-- condition should be variable name-friendly! */
	fixdxcodes=1,		/* forces decimal point (at position 4) if not already present into dx codes with length>3 - also forces to upper case */
	USELIKE=0, 		/* set to 1 if you want to expand codes in your CODEFILE to cover more granular codes in the Dim tables, 
						e.g., 412% LIKE 412.11 rather than 412=412  */
	allowNULLcodes=0,
	xwloc=Src,		/* location of CohortCrosswalk - in general, leave as Src unless a crosswalk was manually created in, e.g., Dflt */
	datsrc=&PROJ,		/* e.g., ord_russo_201509062d */
		dhandle=,
	dftlib=SDAT,
	create_event_file=1,
	create_view=1,			/* create a view (on the SAS side) of the output table (on the SQL Server side) */
		VWLIB=,		/* if not specified, view will be created in the parent directory (on the SAS side) for project <datsrc> */	
	create_sas_event_file=0,	/* if set to 1, creates a SAS dataset (instead of view) in VWLIB - NOTE this will delete the underlying SQL tables! */
	event_file=dxevents,		/* name of permanent dx event-level output table to be created on SQL side (Dflt schema for <datsrc>) */
	event_file_ext=,		/* suffix to be added to end of event_file name -- optional -- in general, you don't need this. */
	create_ptlev_file=1,		/* 0: do not create a pt-level file.  1: apply 1-inpt/2-outpt rule and summarize conditions to person-level */
		ptlev_diagnostics=0,    /* 0: do not make diagnostics variables.  1: use span macro to create diagnostics variables for each condition
						in order to assess how each patient met/did not meet criteria for each condition */
	ip_use_dischdate=1,	/* 0: make date restrictions to inpatient based on ADMISSION date. 1: ...based on DISCHARGE date */
	ipstays=Src.Inpat_Inpatient,
	ipdx=Src.Inpat_InpatientDiagnosis,	/* see note relating to 02 Aug 2022 edit in header */
	opstays=Src.Outpat_Workload,
	opdx=Src.Outpat_WorkloadVDiagnosis,
	primaryonly=1,			/* 0: allow any encounter type (primary, ancillary, occasion of service) for outpat_workload 
					   1: keep only EncounterType='P' from Outpat_Workload -- i.e., exclude ancillary/occasion of service records */
	strict_inpt=1,			/* 0: allow inpt-setting dxs from outpat_workload to be considered as inpatient for purposes of 1 ip/2 op criteria 
					   1: only dxs from inpat_inpatientdiagnosis are considered inpatient */
	/*
	ipdx_req=1,
	opdx_req=2, 			-- not active */
	keepOPfails=0,			/* 1: create a separate set of 'opfail_*' flags that indicate if pt failed IP/OP criteria but did have at least 1 OP */
	dropSQLtable=0,			/* 1: after creating pt-level file, drop underlying (event-level) SQL table to save space ... 0: keep SQL table */
	opdx_minspan=7,			/* for 2-outpatient dx rule, min number of days that can separate the 2 OP dxs -- see the valid_span macro called by this program*/
	opdx_maxspan=365,		/* for 2-outpatient dx rule, max number of days that can separate the 2 OP dxs */
	dxlookback=365			/* applies to EVENT-LEVEL FILE -- number of days prior to index date to look back -- e.g., setting to 3650 will look back 10 years */
	);

	%if &event_file= %then %do;
		%let event_file=dxevents;
	%end;
	%if &create_ptlev_file=0 %then %do;
		%put ::: dropSQLtable was reset to 0 because you selected create_ptlev_file=0 ;
		%let dropSQLtable=0;
	%end;
	%if &dhandle= %then %do;
		%let dhandle=&datsrc;
	%end;
	%let ip_use_ipdx=0;
	%if %lowcase(&ipdx)=src.inpat_inpatientdiagnosis %then %do;
		%let ip_use_ipdx=1;
	%end;
	
	%let srcerror=0;
	%let srcext=;
	%let SrcNum=;

	data _null_;
	length ipstays ipdx opstays opdx $75;
	array d {*} ipstays ipdx opstays opdx;
	ipstays=upcase("&ipstays");
	ipdx=upcase("&ipdx");
	opstays=upcase("&opstays");
	opdx=upcase("&opdx");
	EHRcount=0;
	do i=1 to dim(d);
		if substr(d[i],length(d[i])-3)='_EHR' then do;
			EHRcount+1;
			if substr(d[i],1,4)='SRC.' then do;
				d[i]=tranwrd(d[i],"SRC.","SRC3.");
				call symputx(vname(d[i]),d[i]);
			end;
		end;
	end;
	if EHRcount=0 then call symputx("useSrc3",0);
	else if EHRcount=dim(d) then do;
		call symputx("useSrc3",1);
		call symputx("srcext","_EHR");
		call symputx("SrcNum",3);
	end;
	else call symputx("srcerror",1);
	run;
	
	%if &srcerror %then %do;
		%put ::: ERROR: if any tables have the suffix _EHR then all 4 tables must have this extension ;
		%put ::: tables: &ipstays &ipdx &opstays &opdx;
		%put ::: SAS is quitting! ;
		%abort cancel;
	%end;
	%if &useSrc3 %then %do;
		%put ::: USING SOURCE3 (Vista + Millenium) DATA ;
	%end;

	/*
	%if &event_file_ext^= %then %do;
		%if %substr(&event_file_ext,1,1)^=_ %then %do;
			%let event_file_ext=_&event_file_ext;
		%end;
	%end; */

	%let max_outtab_name_len=21;
	%if &create_ptlev_file=0 %then %do;
		%let max_outtab_name_len=26;
	%end;
	%let OUTTABLE=&event_file&event_file_ext;
	%if %length(&outtable)>&max_outtab_name_len %then %do;
		%put ======================================================================================================= ;
		%put :: WARNING -- your output file name &outtable will be truncated to &max_outtab_name_len chars: %substr(&outtable,1,&max_outtab_name_len);	
		%put ======================================================================================================= ;
		%let OUTTABLE=%substr(&outtable,1,&max_outtab_name_len);
	%end;
	%let PPL=ppl_&outtable;
	%let CODES=codes_&outtable;

	%if (&create_view=1 or &create_ptlev_file=1) and &vwlib= %then %do;
		%put ====================================================================== ;
		%put :: You must specifiy an output lib (with the VWLIB argument)!;
		%put :: SAS is quitting;
		%put ====================================================================== ;
		%abort;
	%end;	

	%if &create_event_file %then %do;

		%include '/data/prod/common/WRJ_macros/js/sql_drop.sas';

		%if &ptfile= %then %do;
			%put ===================================================================== ;
			%put :: You must specify a PTFILE in the macro call! SAS is quitting.... ;
			%put ===================================================================== ;
			%abort;
		%end;
		%if &codefile= %then %do;
			%put ===================================================================== ;
			%put :: You must specify a CODEFILE in the macro call! SAS is quitting.... ;
			%put ===================================================================== ;
			%abort;
		%end;

		%if %length(&CODES)>32 %then %do;
			%put ============================================================================================= ;
			%put :: One of your output table names is too long: &CODES; 
			%put :: Shorten your event_file (&event_file) or event_file_ext (&event_file_ext) and re-run! ;	
			%put ============================================================================================= ;
			%abort;
		%end;

		%put ================================================================================ ;
		%put :: NOTE: Dx event table name: Dflt.&OUTTABLE in project &datsrc;	
		%put :: NOTE: ptlist   table name: Dflt.ppl_&OUTTABLE in project &datsrc;	
		%put :: NOTE: codelist table name: Dflt.codes_&OUTTABLE in project &datsrc;	
		%put ================================================================================ ;

		%if %index(&ptfile,.)=0 %then %let ptfile=WORK.&ptfile;
		%if %index(&codefile,.)=0 %then %let codefile=WORK.&codefile;
		
		* check input files ;
		%let ptvarsOK=0;
		%let ptstructureOK=0;
		%let missfmt=0;

		proc sql undo_policy=NONE noprint;
		** check ptfile ;
		select (count(*)=2) into :ptvarsOK from dictionary.columns where 
		upcase(name) in ("PATIENTICN","INDEXDATE") and libname=upcase("%scan(&ptfile,1,.)")
		and upcase(memname)=upcase("%scan(&ptfile,2,.)");
		
		%if &ptvarsOK %then %do;
			select (max(nrecs)=1) into :ptstructureOK 
			from
				(select distinct patientICN, indexdate, count(*) as nrecs from 
				&ptfile group by patientICN, indexdate);

			select (substr(format,1,8)="DATETIME") into :dttime 
			from dictionary.columns where 
			libname=upcase("%scan(&ptfile,1,.)") and upcase(memname)=
			upcase("%scan(&ptfile,2,.)") and upcase(name)="INDEXDATE";

			select missing(format) into :missfmt 
			from dictionary.columns where 
			libname=upcase("%scan(&ptfile,1,.)") and upcase(memname)=
			upcase("%scan(&ptfile,2,.)") and upcase(name)="INDEXDATE";

		%end;
		
		** check codefile ;
		select (count(*)=3) into :codevarsOK from dictionary.columns where 
		upcase(name) in ("CODETYPE","CODE","CONDITION") and libname=upcase("%scan(&codefile,1,.)")
		and upcase(memname)=upcase("%scan(&codefile,2,.)");

		select count(*) into :nNULLcodes from &codefile where missing(code);

		select count(*) into :badtypes from &codefile where upcase(codetype) not in ('DX09', 'DX10');
		quit;

		%if &badtypes %then %do;
			data &codefile;
			set &codefile;
			if upcase(codetype)='DX9' then codetype='Dx09';
			run;

			proc sql undo_policy=NONE noprint;
			select count(*) into :badtypes from &codefile where upcase(codetype) not in ('DX09', 'DX10');
			quit;
		%end;

		data _null_;
		call symputx("allOK",&ptvarsOK*&ptstructureOK*(&missfmt=0)*&codevarsOK*(&badtypes=0));
		run;

		%if &allOK=0 %then %do;
			%put :: STOPPING! Your input patient and code files were not valid!;
			%put :: PTVARSOK: &ptvarsOK  PTSTRUCTUREOK: &ptstructureOK  MISSFMT: &missfmt  CODEVARSOK: &codevarsOK  BADTYPES: &badtypes;
			%abort cancel;
		%end;

		* do some diagnostics on the ptfile and codefile ;
		%if &nNULLcodes %then %do;
			%if &allowNULLcodes=0 %then %do;
				%put :: ERROR: your code list &codefile contains missing values for CODE but you have not set allowNULLcodes to 1! ;
				%put :: SAS is quitting;
				%abort cancel;
			%end;
			* force USELIKE to 1 ;
			%let USELIKE=1;
		%end;

		* push the ptfile and codefile to the SQL side ;
		libname user sqlsvr schema=Dflt &SQL_OPTIMAL;
		
		proc sql undo_policy=NONE;
		connect to sqlsvr as user (datasrc=&dhandle &sql_optimal);
		/*
		*execute(%dropit(Dflt.&PPL);) by user;

		*execute(%dropit(Dflt.&CODES);) by user; */

		execute(drop table if exists Dflt.&PPL;) by user;
		execute(drop table if exists Dflt.&CODES;) by user;

		disconnect from user;
		quit;

		%put ::: PTFILE: &ptfile;
		%put ::: CODEFILE: &codefile;
		%put ::: DFTLIB: &dftlib;
		%put ::: DTTIME: &dttime;
		%put ::: PPL: &PPL;
	
		data &dftlib..&PPL (dbtype=(patientICN='varchar(10)')) ;
		set &ptfile (rename=(indexdate=idttime));
		length indexdate 8;
		format indexdate datetime20.;
		if &dttime then indexdate=idttime;
		else indexdate=dhms(idttime,0,0,0);  
		keep patientICN indexdate;
		run;

		data &dftlib..&CODES
			(dbtype=(
				code='varchar(10)'
				codetype='varchar(6)'
				condition='varchar(32)'
			));
		set &codefile;
		keep code codetype condition;
		codetype=propcase(codetype); * added 16 Sep 2020 ;
		%if &fixdxcodes %then %do;
			code=upcase(code);
			if upcase(substr(codetype,1,2))='DX' and index(code,'.')=0 then do;
				if length(code)>3 then do;
					code=compress(substr(code,1,3) || '.' || substr(code,4));
					output;
				end;
				else if length(code)=3 then do;
					output;  /* w/o trailing decimal */
					code=compress(code || '.');
					output;  /* same code, with trailing decimal to suit CDW weirdness */
				end;
				else output; /* there should be no records output here */
			end;
			else output;
		%end;
		run;

		title "first 10 obs of codefile &codefile";
		proc print data=&codefile (obs=10) width=min; run;
		title;

		proc sql undo_policy=NONE;
		connect to sqlsvr as user (datasrc=&dhandle &sql_optimal);

		/*execute(use [&datsrc];) by user;*/

		execute(drop table if exists Dflt.&OUTTABLE;) by user;

		execute(
			create table Dflt.&OUTTABLE 
			(patientICN VARCHAR(10), indexdate DATETIME2(0), patientSID BIGINT, sta3n SMALLINT, 
			inpat_or_vis_SID BIGINT, admit_or_vis_datetime DATETIME2(0),
			disch_or_vis_datetime DATETIME2(0), PrStopCode VARCHAR(6), PrStopCodeName VARCHAR(30), 
			SERVICECATEGORY VARCHAR(8), ENCOUNTERTYPE VARCHAR(8), dxpos VARCHAR(1), FACTYPE VARCHAR(8), 
			condition VARCHAR(32), %if &ip_use_ipdx=1 %then POAindicator VARCHAR(4),; 
			CODETYPE VARCHAR(10), CODE VARCHAR(10), RAWCODE VARCHAR(10));

			declare @LBstart datetime2(0)
			declare @LBend datetime2(0)

			/* this will just help roughly subset the data to only records that are no older than <dxlookback> days prior to the earliest index date */
			set @LBstart=dateadd(dd,-&dxlookback,(select min(indexdate) from Dflt.&PPL))
			set @LBend=dateadd(dd,0,(select max(indexdate) from Dflt.&PPL));

			INSERT INTO Dflt.&OUTTABLE WITH (TABLOCK)	/* this option helps avoid filling up the transaction log */ 
			(patientICN, indexdate, patientSID, sta3n, inpat_or_vis_SID, admit_or_vis_datetime, disch_or_vis_datetime, PrStopCode, PrStopCodeName,
			servicecategory, encountertype, dxpos, factype, condition, %if &ip_use_ipdx=1 %then POAindicator,; 
			codetype, code, rawcode) 

				select DISTINCT SHELL.patientICN, SHELL.indexdate, SHELL.patientSID, SHELL.sta3n, 
				SHELL.inpat_or_vis_SID, SHELL.admit_or_vis_datetime, 
				SHELL.disch_or_vis_datetime, SHELL.PrStopCode, SHELL.PrStopCodeName, SHELL.servicecategory, SHELL.encountertype,
				SHELL.dxpos, SHELL.FACTYPE, SHELL.condition, %if &ip_use_ipdx=1 %then SHELL.POAindicator,; 
				SHELL.codetype, SHELL.code, SHELL.rawcode from

				( 
				select ASIDE.patientICN, ASIDE.inpatientSID as inpat_or_vis_SID, ASIDE.patientSID, ASIDE.sta3n,
				ASIDE.indexdate, ASIDE.admitdatetime as admit_or_vis_datetime, 
				ASIDE.dischargedatetime as disch_or_vis_datetime, NULL as PrStopCode, NULL as PrStopCodeName, 
				ASIDE.servicecategory, ASIDE.encountertype, case when pdxsid=codesid then 'P' else 'S' end as dxpos,
				ASIDE.FACTYPE, DSIDE.condition, DSIDE.codesid, %if &ip_use_ipdx=1 %then DSIDE.POAindicator,; 
				DSIDE.codetype, DSIDE.code, DSIDE.rawcode from


					/* Aside (admissions) -- inpatient -- join inpt stay table to cohort via cohort xwalk */
					(select STAYS.patientSID, STAYS.sta3n, STAYS.inpatientsid, STAYS.servicecategory, STAYS.encountertype, STAYS.pdxsid,
					STAYS.FACTYPE, STAYS.admitdatetime, STAYS.dischargedatetime, COHX.patientICN, COHX.indexdate from
						(select patientsid, sta3n, inpatientsid, 
							cast('z' as VARCHAR(8)) as servicecategory, cast('z' as varchar(8)) as encountertype,
							cast('INPT' as VARCHAR(8)) as FACTYPE, 
							admitdatetime, dischargedatetime,
							case 
								when principaldiagnosisICD10SID>1 then principaldiagnosisICD10SID 
								else principaldiagnosisICD9SID 
							end as pdxsid
							
							from &IPSTAYS WHERE 
								%if &ip_use_dischdate %then dischargedatetime>=@LBstart and dischargedatetime<=@LBend;
								%else admitdatetime>=@LBstart and admitdatetime<=@LBend;
								) STAYS
						INNER JOIN		
						
						(select distinct coh.patientICN, coh.indexdate, ptxw.patientsid, ptxw.sta3n
							from Dflt.&PPL coh inner join &xwloc..CohortCrosswalk ptxw
							on coh.patientICN=ptxw.patientICN) COHX
						ON STAYS.patientsid=COHX.patientsid and STAYS.sta3n=COHX.sta3n and
						datediff(d,%if &ip_use_dischdate %then STAYS.dischargedatetime; %else STAYS.admitdatetime;, COHX.indexdate) 
						between 0 and &DXLOOKBACK) ASIDE
					/* End of Aside -- inpatient */

					INNER JOIN

					/* Dside (dx codes) -- inpatient -- join inpt disch dx to condition list via icd9/10 dim tables */
					(select DISCHDX.patientsid, DISCHDX.sta3n, DISCHDX.inpatientsid, DISCHDX.codetype, DISCHDX.codesid, 
					%if &ip_use_ipdx=1 %then DISCHDX.POAindicator,;
					CODES.code, CODES.condition, CODES.rawcode from		
						(select  patientsid, sta3n, inpatientsid, 'Dx09' as codetype,
							icd9sid as codesid %if &ip_use_ipdx=1 %then , POAindicator;
							from &IPDX WHERE 
								%if &ip_use_dischdate %then dischargedatetime>=@LBstart and dischargedatetime<=@LBend;
								%else admitdatetime>=@LBstart and admitdatetime<=@LBend; 
								and icd9sid>0
							UNION ALL
							select patientsid, sta3n, inpatientsid, 'Dx10' as codetype,
							icd10sid as codesid %if &ip_use_ipdx=1 %then , POAindicator;
							from &IPDX WHERE 
								%if &ip_use_dischdate %then dischargedatetime>=@LBstart and dischargedatetime<=@LBend; 
								%else admitdatetime>=@LBstart and admitdatetime<=@LBend; 
								and icd10sid>0) DISCHDX
						INNER JOIN

						(select distinct codelist.code, codelist.codetype, codelist.condition, 
						codestack.codesid, codestack.sta3n, codestack.rawcode 
							from 
								%if &allowNULLcodes %then %do;
									(select case when code is NULL then '' else code end as code, 
									codetype, condition from Dflt.&CODES) 
								%end;
								%else %do; 
									Dflt.&CODES
								%end; codelist
							INNER JOIN

								(select 'Dx09' as codetype, icd9code as rawcode, icd9sid as codesid, sta3n
								from CDWWork&SrcNum..Dim.ICD9&SrcExt
								UNION ALL
								select 'Dx10' as codetype, icd10code as rawcode, icd10sid as codesid, sta3n
								from CDWWork&SrcNum..Dim.ICD10&SrcExt) codestack
							ON codelist.codetype=codestack.codetype and codestack.rawcode 
								%if &uselike %then LIKE codelist.code+'%'; %else = codelist.code;) CODES
								
						ON DISCHDX.codetype=CODES.codetype and DISCHDX.codesid=CODES.codesid and DISCHDX.sta3n=CODES.sta3n) DSIDE

					/* End of Dside -- inpatient */

					ON ASIDE.inpatientsid=DSIDE.inpatientsid and ASIDE.sta3n=DSIDE.sta3n

				/* End of LEFTSIDE Inpatient stuff */

			UNION ALL

				/* Start of LEFTSIDE Outpatient stuff */

				select ASIDE.patientICN, ASIDE.visitSID as inpat_or_vis_SID, ASIDE.patientSID, ASIDE.sta3n,
				ASIDE.indexdate, ASIDE.visitdatetime as admit_or_vis_datetime, 
				ASIDE.visitdatetime as disch_or_vis_datetime, ASIDE.StopCode as PrStopCode, ASIDE.StopCodeName as PrStopCodeName, 
				ASIDE.SERVICECATEGORY, ASIDE.ENCOUNTERTYPE, DSIDE.dxpos,
				ASIDE.FACTYPE, DSIDE.condition, DSIDE.codesid, %if &ip_use_ipdx=1 %then NULL as POAindicator,;
				DSIDE.codetype, DSIDE.code, DSIDE.rawcode from


					/* Aside (visits) -- outpatient -- join outpat visit table to cohort via cohort xwalk */
					(select STAYS.patientSID, STAYS.sta3n, STAYS.visitSID, STAYS.servicecategory, STAYS.encountertype, STAYS.FACTYPE, STAYS.visitdatetime, 
					COHX.patientICN, COHX.indexdate, SC.StopCode, SC.StopCodeName from
						(select patientsid, sta3n, visitSID, servicecategory, encountertype, primarystopcodeSID,
						cast((case when servicecategory in ('D','H','I') then 'inpt' else 'OPAT' end) as VARCHAR(8)) as FACTYPE, visitdatetime
						from 
						&OPSTAYS WHERE visitdatetime>=@LBstart and visitdatetime<=@LBend %if &primaryonly %then and encountertype='P';) STAYS
						INNER JOIN		
						
						(select distinct coh.patientICN, coh.indexdate, ptxw.patientsid, ptxw.sta3n
							from Dflt.&PPL coh inner join &xwloc..CohortCrosswalk ptxw
							on coh.patientICN=ptxw.patientICN) COHX
						ON STAYS.patientsid=COHX.patientsid and STAYS.sta3n=COHX.sta3n and
						datediff(d,STAYS.visitdatetime, COHX.indexdate) between 0 and &DXLOOKBACK
						%if &useSrc3 %then LEFT; %else INNER; JOIN
					
						CDWWork&SrcNum..Dim.StopCode&SrcExt SC
						on STAYS.primarystopcodeSID=SC.stopcodeSID and STAYS.sta3n=SC.sta3n) ASIDE
					/* End of Aside -- outpatient */

					INNER JOIN

					(
					select DSIDEsub.patientSID, DSIDEsub.sta3n, DSIDEsub.VISITsid, 
					DSIDEsub.condition, DSIDEsub.codesid,	DSIDEsub.dxpos, DSIDEsub.codetype, 
					DSIDEsub.code, DSIDEsub.rawcode 
					from
						/* Join cohort to cohort xwalk to make COHX, then join this to outpatient DSIDE created below */
							(select distinct coh.patientICN, coh.indexdate, ptxw.patientsid, ptxw.sta3n
								from Dflt.&PPL coh inner join &xwloc..CohortCrosswalk ptxw
								on coh.patientICN=ptxw.patientICN) COHX
							INNER JOIN

						/* DSIDEsub -- outpatient -- join outpatient VDiagnosis table to condition list via icd9/10 dim tables */
						(select DISCHDX.patientsid, DISCHDX.sta3n, DISCHDX.visitSID, DISCHDX.visitdatetime, DISCHDX.codetype, DISCHDX.codesid,
						DISCHDX.dxpos, CODES.code, CODES.condition, CODES.rawcode from		
							(select patientsid, sta3n, visitSID, visitdatetime, 'Dx09' as codetype,
								icd9sid as codesid, PrimarySecondary as dxpos
								from &OPDX WHERE visitdatetime>=@LBstart and visitdatetime<=@LBend and icd9sid>0 
								UNION ALL
								select patientsid, sta3n, visitSID, visitdatetime, 'Dx10' as codetype,
								icd10sid as codesid, PrimarySecondary as dxpos
								from &OPDX WHERE visitdatetime>=@LBstart and visitdatetime<=@LBend and icd10sid>0) DISCHDX
							INNER JOIN

							(select distinct codelist.code, codelist.codetype, codelist.condition, codestack.rawcode, codestack.codesid, codestack.sta3n 
								from 
								%if &allowNULLcodes %then %do;
									(select case when code is NULL then '' else code end as code, 
									codetype, condition from Dflt.&CODES) 
								%end;
								%else %do; 
									Dflt.&CODES
								%end; codelist
								INNER JOIN

									(select distinct 'Dx09' as codetype, icd9code as rawcode, icd9sid as codesid, sta3n
									from CDWWork&SrcNum..Dim.ICD9&SrcExt
									UNION ALL
									select distinct 'Dx10' as codetype, icd10code as rawcode, icd10sid as codesid, sta3n
									from CDWWork&SrcNum..Dim.ICD10&SrcExt) codestack
								ON codelist.codetype=codestack.codetype and codestack.rawcode
									%if &uselike %then LIKE codelist.code+'%'; %else = codelist.code;) CODES
									
							ON DISCHDX.codetype=CODES.codetype and DISCHDX.codesid=CODES.codesid and DISCHDX.sta3n=CODES.sta3n) DSIDEsub
						/* End of DSIDEsub -- outpatient */

						ON COHX.patientsid=DSIDEsub.patientsid and COHX.sta3n=DSIDEsub.sta3n and
						datediff(d,DSIDEsub.visitdatetime, COHX.indexdate) between 0 and &DXLOOKBACK) DSIDE

					ON ASIDE.patientSID=DSIDE.patientSID and ASIDE.sta3n=DSIDE.sta3n and ASIDE.visitSID=DSIDE.visitSID	
						) SHELL;

		) by user;

		disconnect from user;
		quit;

	%end; *create _event_file;

	%if &dropSQLtable=1 %then %do;
		%let create_view=0;
		%let keepOPfails=1;
	%end;
	
	%if &create_view or &create_sas_event_file %then %do;
		%if &vwlib= %then %do;
			libname vwlib "/data/dart/%substr(%scan(&datsrc,-1,_),1,4)/&datsrc";  /* note this only works for <datsrc> in the form 'xxx_<PIname>_YYYYMMDDxd' ! */
			%let vwlib=vwlib;
			%put :: NOTE: no library specified for output view -- set to parent directory for project &datsrc;
		%end;

		proc sql undo_policy=NONE;
		connect to sqlsvr as user (datasrc=&dhandle &sql_optimal);

		/*execute(use [&datsrc];) by user; */
		create %if &create_sas_event_file %then table; %else view; &vwlib..&OUTTABLE as 
		select  
			patientICN 		length=10
			, indexdate		length=8
			, patientSID		length=8
			, sta3n			length=4
			, inpat_or_vis_SID 	length=8
			, admit_or_vis_datetime length=8
			, disch_or_vis_datetime length=8
			, prstopcode		length=6
			, prstopcodename	length=30
			, servicecategory	length=8
			, encountertype		length=8
			, dxpos			length=1
			, factype		length=8
			, condition		length=32
			%if &ip_use_ipdx=1 %then %do;
				, POAindicator	length=1
			%end;
			, codetype		length=10
			, code			length=10
			, rawcode		length=10
		from connection to user (
			select * from Dflt.&OUTTABLE);

		%if &create_sas_event_file %then %do;
			* since a SAS dataset was created (as opposed to a view), drop the SQL tables ;
			execute(drop table if exists Dflt.&PPL;) by user;
			execute(drop table if exists Dflt.&CODES;) by user;
			execute(drop table if exists Dflt.&OUTTABLE;) by user;
		%end;

		disconnect from user;
		quit;
	%end; *create_view;

	%if &create_ptlev_file %then %do;

		%let PTINDLEV=ptindexlev_&outtable;

		%if &ptfile= %then %do;
			%put ===================================================================== ;
			%put :: You must specify a PTFILE in the macro call! SAS is quitting.... ;
			%put ===================================================================== ;
			%abort;
		%end;
		%if &codefile= %then %do;
			%put ===================================================================== ;
			%put :: You must specify a CODEFILE in the macro call! SAS is quitting.... ;
			%put ===================================================================== ;
			%abort;
		%end;

		data WORK.rawevents (rename=(indate=indexdate));
		set %if &create_sas_event_file %then &vwlib..&OUTTABLE; %else &dftlib..&OUTTABLE;;
		length inpt 3 evdate indate 5;
		%if &strict_inpt=0 %then %do;
			factype=upcase(factype); /* less strict 'inpatient' definition allows inpt-setting dxs 
						from outpat_workload to be considered as inpt for purposes of 1 ip/2 op criteria */
		%end;
		inpt=(factype='INPT');
		%if &ip_use_dischdate %then %do;
			evdate=datepart(disch_or_vis_datetime);
		%end;
		%else %do;
			evdate=datepart(admit_or_vis_datetime);
		%end;
		indate=datepart(indexdate);
		drop indexdate;
		format evdate indate yymmddn8.;
		run;

		%include '/data/prod/common/WRJ_macros/valid_span.sas';

		proc sort data=WORK.rawevents; by patientICN indexdate; run;

		data WORK.rawevents;
		set WORK.rawevents;
		by patientICN indexdate;
		length n_index 3;
		if first.patientICN then n_index=0;
		n_index+first.indexdate;
		run;

		proc sql undo_policy=NONE noprint;
		select max(n_index) into :maxn from WORK.rawevents;
		quit;

		proc datasets lib=work memtype=data nolist nodetails;
		delete everyone_everyindex;  
		run; quit;

		%let diagnostics_var=;
		%if &ptlev_diagnostics %then %let diagnostics_var=codetype;

		%if &maxn>1 %then %do;
			%put ::: NOTE: some people had up to %trim(&maxn) distinct index dates - these people will appear multiple times in ptindexlev file ;
		%end;

		%do NI=1 %to &maxn;
			
			data WORK.raw_i;
			set WORK.rawevents;
			WHERE n_index=&NI;
			run;

			%span(
				WORK.raw_i,
				personvar=patientICN,
				datevar=evdate,
				splitbyvar=condition,
				eventsource=&diagnostics_var,
				minspan=&opdx_minspan,
				maxspan=&opdx_maxspan,
				keepOPfails=&keepOPfails,
				sameyear=0,
				firstlast=FIRST,
				flipOP=0,
				personlevvars=indexdate
				);

			* the above macro creates a person-level temporary SAS dataset
			called 'EVERYONE';

			%if &NI=1 %then %do;
				data WORK.everyone_everyindex;
				set WORK.everyone;
				run;
			%end;
			%else %do;
				data WORK.everyone_everyindex;
				set
					WORK.everyone_everyindex
					WORK.everyone
					;
				run;
			%end;

			proc datasets lib=work memtype=data nolist nodetails;
			delete raw_i everyone;  
			run; quit;

		%end; *maxn loop;

		proc datasets lib=work memtype=data nolist nodetails;
		delete rawevents;  * note we are not deleting the SQL table ;
		run; quit;
	
		** this is a crude check to determine whether indexdate in <PTFILE> is a date or datetime ;
		proc sql undo_policy=NONE noprint;
		select (year(max(indexdate))=.) into :dttime from &ptfile;
		quit;	

		** create a shell table called ptfile_temp using the <ptfile> and <codefile> containing all ppl/indexdates/conditions ;
		data WORK.ptfile_temp;
		set &ptfile;
		%if &dttime %then %do;
			indexdate=datepart(indexdate);
		%end;
		run;

		proc sort data=WORK.ptfile_temp 
			NODUPKEY;
		by patientICN indexdate;
		run;

		proc sql undo_policy=NONE noprint;
		select distinct compress("conf_" || upcase(condition)) into :cvars separated by " " from &codefile;
		quit;

		data WORK.ptfile_temp;
		length patientICN $10 indexdate 5 indexdate_num 3 n_conf 3 &cvars 3;
		set WORK.ptfile_temp;
		by patientICN indexdate;
		if first.patientICN then indexdate_num=0;
		indexdate_num+1;
		n_conf=.;
		array c {*} &cvars;
		do i=1 to dim(c);
			c[i]=.;
		end;
		drop i;
		run;

		proc sql undo_policy=NONE;
		title "Missing the following conditions from the output table...";
		select name from dictionary.columns 
			where libname='WORK' and lowcase(memname)='ptfile_temp' and lowcase(substr(name,1,5))='conf_'
		except
		select name from dictionary.columns 
			where libname='WORK' and lowcase(memname)='everyone_everyindex' and lowcase(substr(name,1,5))='conf_';
		quit;
		title;

		proc sort data=WORK.everyone_everyindex
			NODUPKEY;
		by patientICN indexdate;
		run;

		data WORK.everyone_everyindex;
		merge
			WORK.ptfile_temp (in=A)
			WORK.everyone_everyindex (in=B)
			;
		by patientICN indexdate;
		IF A;
		array c {*} conf_:;
		do i=1 to dim(c);
			c[i]=(c[i]=1);
		end;
		n_conf=sum(of conf_:);
		format indexdate date9.;
		drop i;
		run;

		title "summary of conditions by index date number";
		proc means data=WORK.everyone_everyindex nmiss min max mean sum maxdec=3;
		class indexdate_num;
		var n_conf conf_:;
		run;
		title;

		* save a permanent patient~indexdate-level file ;
		data &vwlib..&ptindlev (label=&sasprog);
		set WORK.everyone_everyindex;
		run;
	
		%if &dropSQLtable=1 %then %do;
			proc sql undo_policy=NONE;
			connect to sqlsvr as user (datasrc=&dhandle &sql_optimal);
			execute(drop table if exists Dflt.&PPL;) by user;
			execute(drop table if exists Dflt.&CODES;) by user;
			execute(drop table if exists Dflt.&OUTTABLE;) by user;
			disconnect from user;
			quit;
		%end;

	%end; *create_ptlev_file;
	
%MEND; *getDxPx();


