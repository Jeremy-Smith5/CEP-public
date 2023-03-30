
/*
PROGRAM: pull.radiology_text.sas

Relatively crude first pass through ReportText field in STIUNotes_TIU_8925 view - 
first subsetting to a given month, then to anything containing 'lung', then 
looking for more cancer-specific things, then appending to stack of 
previous months, then saving a permanent SAS dataset.

*****************************************************************************************
UPDATE:  this 'radiology_text' version does ~the same as the TIU version described above
but instead pulls report text from the Radiology domain.
*****************************************************************************************

vhawrjsmithj
04 Jan 2023

================================================================================================ */

*OPTIONS OBS=10000;
title; title2;
options nocenter nonumber nodate mprint mergeNoBy=error formdlim=' ' pagesize=max linesize=180;

%let COMMON=/data/prod/common/WRJ_macros;
%include "&COMMON/mk.globals.sas";
%include "&COMMON/chk.lib.access.sas"; /*ex: %chklib(/home/test/plots); */
%include "&COMMON/count.sas"; /*outputs: WORK.allfile_counts - ex: %count(ptadmlev, select=pt admdt, dupfail=1, step=chk if pt-admit level); */
/*
GLOBAL MACRO VARIABLES: 
.......&SASPROG:  full path SAS program name
.......&SASLOG:   full path SAS log name
.......&PROJROOT: full path through ORD project
.......&PROJ:     ORD project
.......&DFLTLIB:  SQL Server libname for Dflt path
.......&PDRIVE:   absolute path to relevant P: drive folder
.......&CWD:      full path to directory for this program (also defined as libname CWD)
SQL SERVER: proc sql; connect to SQLSVR as SDAT (DATASRC=&proj &SQL_OPTIMAL);
SQL SERVER: ...select * from connection to SDAT(...); disconnect from SDAT  */

libname sdat sqlsvr datasrc=&PROJ &sql_optimal schema=dflt;
libname feas "&PROJROOT/Data/feas";

%MACRO pull_Rad(
	globalstart,
	dtstart, /* inclusive */
	dtend,   /* exclusive */
	ptlist=, /* any SAS dataset containing patientICNs of interest - does not need to be pt level */
	check_n=0, /* if set to 1, just check total # of STIUNotes records for the first time window, then stop */
	outdata=,
	TIUdocdef=, /* optional restriction placed on inner join to Dim.TIUDocumentDefinition -- example:  %str(WHERE TIUDocumentDefinition like '%LUNG%') */
	where1=, 
	where2=,
	where3=
	);

	%PUT ::: DTRANGE: &dtstart - &dtend;

	%if %symexist(firstpass)=0 %then %do;

		proc sort data=&ptlist out=_ptlist (keep=patientICN) NODUPKEY; by patientICN; run;

		proc sql;
		connect to sqlsvr as sdat (datasrc=&PROJ &SQL_OPTIMAL);

		execute(drop table if exists #ptlist) by sdat;
		execute(drop table if exists #ptx) by sdat;

		disconnect from sdat;
		quit;

		data sdat.'#ptlist'n (dbtype=(patientICN='varchar(50)'));
		set _ptlist;
		run;

		%global firstpass;
		%let firstpass=1;

	%end;

	proc sql;
	connect to sqlsvr as sdat (datasrc=&PROJ &SQL_OPTIMAL DBMAX_TEXT=16384);

	execute(drop table if exists #lungTS) by sdat;

	%if &firstpass %then %do;
		execute(
			select distinct a.patientICN, a.patientSID
			into #ptx
			from
				[&PROJ].[Src].[CohortCrosswalk] A
				inner join
				#ptlist B
				on a.patientICN=b.patientICN;

			drop table if exists #ptlist;
			
			) by sdat;
		%let firstpass=0;
	%end;

	%if &check_n %then %do;
		* just check total number of records in STIUNotes for the first time window for patientSIDs 
		that were in the provided ptlist ;
		execute(
			declare @startdt datetime2(0);
			declare @enddt datetime2(0);

			set @startdt=cast(%nrbquote(')&dtstart.T00:00:00.000%nrbquote(') as datetime2(0));
			set @enddt=cast(%nrbquote(')&dtend.T00:00:00.000%nrbquote(') as datetime2(0));

			select count(b.patientSID) as nrecs into #lungTS
			from
				#ptx A
				inner join
				(
				select patientSID from [&PROJ].[src].[SPatientText_RadiologyImpressions]
				WHERE 
					reportEntereddatetime>=@startdt
					and
					reportEntereddatetime< @enddt
				union all
				select patientSID from [&PROJ].[src].[SPatientText_RadiologyReportText]
				WHERE 
					reportEntereddatetime>=@startdt
					and
					reportEntereddatetime< @enddt) B
				on a.patientSID=b.patientSID) by sdat;

		select nrecs into :totrecs from connection to sdat(select nrecs from #lungTS);

		%put ::: There were &totrecs STIUNotes records for the time period &dtstart - &dtend ;

		endsas;

	%end;

	%else %do;
		* pull data ;

		execute(
			declare @globalstart datetime2(0);
			declare @startdt datetime2(0);
			declare @enddt datetime2(0);

			set @globalstart=cast(%nrbquote(')&globalstart.T00:00:00.000%nrbquote(') as datetime2(0));
			set @startdt=cast(%nrbquote(')&dtstart.T00:00:00.000%nrbquote(') as datetime2(0));
			set @enddt=cast(%nrbquote(')&dtend.T00:00:00.000%nrbquote(') as datetime2(0));

			select a.patientICN, re.examDateTime, b.patientSID, b.radsrc, b.radiologyNuclearMedicineReportSID, 
			b.reportEntereddatetime, b.reportText, drp.radiologyProcedure, drp.procedureType, dc.CPTcode
			into #lungTS
			from 
				#ptx A
				inner join
				(select patientSID, examdatetime, radiologynuclearmedicinereportSID, radiologyProcedureSID, 
				radiologydiagnosticCodeSID
				FROM [ORD_Davies_202110040D].[Src].[Rad_RadiologyExam] 
				WHERE
					examDateTime>=@startdt
					and
					examDateTime< @enddt) re
				on a.patientSID=re.patientSID
				inner join
				[CDWWork].[Dim].[RadiologyProcedure] drp
				on re.radiologyprocedureSID=drp.radiologyprocedureSID
				left join
				[CDWWork].[Dim].[CPT] dc
				on drp.cptsid=dc.cptsid
				inner join
				(
				select patientSID, 'Imp' as radsrc, 
				radiologyNuclearMedicineReportSID, reportEntereddatetime, impressionText as reportText
				from [&PROJ].[src].[SPatientText_RadiologyImpressions]
				WHERE reportEntereddatetime>=@globalstart
				union all
				select patientSID, 'Rad' as radsrc, 
				radiologyNuclearMedicineReportSID, reportEntereddatetime, reportText
				from [&PROJ].[src].[SPatientText_RadiologyReportText]
				WHERE reportEntereddatetime>=@globalstart) B
				on re.patientSID=b.patientSID
				and 
				re.radiologynuclearmedicineReportSID=b.radiologynuclearmedicineReportSID
				) by sdat;

		execute(
			select patientICN, patientSID, examDateTime, radsrc, radiologyNuclearMedicineReportSID, reportEntereddatetime, 
			reportText, radiologyProcedure, procedureType, CPTCode
			into #tiuW1
			from #lungTS
			WHERE
				&WHERE1
				) by sdat;

		execute(drop table if exists #lungTS) by sdat;

		%let tempname=#tiuW1;

		%if %length(&where2) %then %do;

			execute(
				select patientICN, patientSID, examDateTime, radsrc, radiologyNuclearMedicineReportSID, reportEntereddatetime, 
				reportText, radiologyProcedure, procedureType, CPTCode
				into #tiuW2
				from #tiuW1
				WHERE
					&WHERE2
					) by sdat;

			execute(drop table if exists #tiuW1) by sdat;

			%let tempname=#tiuW2;

			%if %length(&where3) %then %do;
		
				execute(
				select patientICN, patientSID, examDateTime, radsrc, radiologyNuclearMedicineReportSID, reportEntereddatetime, 
				reportText, radiologyProcedure, procedureType, CPTCode
					into #tiuW3
					from #tiuW2
					WHERE
						&WHERE3
						) by sdat;

				execute(drop table if exists #tiuW2) by sdat;

				%let tempname=#tiuW3;

			%end;

		%end;

		create table &outdata as
		select patientICN length=10, patientSID, examDateTime, radsrc length=3,
 		radNucMedReportSID, reportEntereddatetime, reportText length=16384, radiologyProcedure length=50, procedureType length=30, CPTCode length=5
		from connection to sdat (
			select distinct patientICN, patientSID, examDateTime, radsrc, 
			radiologyNuclearMedicineReportSID as radNucMedReportSID, reportEntereddatetime, reportText, radiologyProcedure, procedureType, CPTCode
			from &tempname);

		execute(drop table if exists &tempname) by sdat;

	%end; * pull data ;

	disconnect from sdat;

	quit;

%MEND; *pull_Rad();

%MACRO dateloop(firststart=, laststart=, interval=month, intmult=1, dxeventfile=, dxeventdatevar=%str(datepart(admit_or_vis_datetime)));
	
	proc format;
	value fn2m
		1='Jan'
		2='Feb'
		3='Mar'
		4='Apr'
		5='May'
		6='Jun'
		7='Jul'
		8='Aug'
		9='Sep'
		10='Oct'
		11='Nov'
		12='Dec'
		;
	run;

	%let done=0;

	data _null_;
	* convert to integer ;
	fs="&firststart"d;
	call symputx("currStart",fs);
	call symputx("globalStartSQL",catx('-', put(year(fs),z4.), put(month(fs),z2.), put(day(fs),z2.)));
	run;

	%let wnum=0;

	%do %while (&done=0);

		%let wnum=%eval(&wnum+1);

		data _null_;
		call symputx("currEnd",intnx("&interval",&currStart,&intmult));
		run;

		data _null_;
		call symputx("currStartSQL",catx('-', put(year(&currStart),z4.), put(month(&currStart),z2.), put(day(&currStart),z2.)));
		call symputx("currEndSQL",catx('-', put(year(&currEnd),z4.), put(month(&currEnd),z2.), put(day(&currEnd),z2.)));
		run;

		%pull_Rad(
			globalstart=&globalStartSQL,
			dtstart=&currStartSQL, /* inclusive */
			dtend=&currEndSQL,   /* exclusive */
			ptlist=firstlungdx,
			check_n=0,
			outdata=radmonth,
			where1=%str(reportText like '%lung%'), 
			where2=%str(reportText like '%tumor%' or reportText like '%nodule%' or reportText like '%mass%' 
				or reportText like '%lesion%' or reportText like '%neoplasm%' or reportText like '%carcinoma%'
				or reportText like '%spiculate%'),
			where3=
			);

		data radmonth;
		length wnum 3 wstart wend 4;
		set radmonth end=last;
		format wstart wend date9.;
		wnum=&wnum;
		wstart=&currstart;
		wend=&currend-1;

		if last then do;
			put '::: chunk:     ' wnum;
			put '::: currstart: ' wstart;
			put '::: currend:   ' wend;
		end;
		run;

		proc append data=radmonth base=all_radmonth; run;

		proc sql;
		drop table radmonth;
		quit;	
		
		%let currStart=&currEnd;

		data _null_;
		call symputx("done",(&currstart>"&laststart"d));
		run;

		%if &syscc>=7 %then %do;
			%let done=1;
		%end;

	%end;

	data _null_;
	lastend=&currEnd-1;
	call symputx("lastend",compress(put(day(lastend),z2.) || put(month(lastend),fn2m.) || year(lastend)));
	run;

	proc sql;
	create table all_radmonth as
	select a.*, (b.firstlungdx-datepart(a.examDateTime)) as lungdxdt_minus_repdt length=4
	from
		all_radmonth A
		inner join
		(select patientICN, min(&dxeventdatevar) as firstlungdx length=5
		from &dxeventfile group by patientICN) B
		on a.patientICN=b.patientICN;
	quit;

	data feas.rad_raw_pull_&firststart._&lastend;
	set all_radmonth;
	run;

	proc sql;
	drop table all_radmonth;
	quit;

%MEND; *dateloop();

/*
proc sort data=feas.lungca_mar2016feb2022
	out=firstlungdx (keep=patientICN admit_or_vis_datetime);
by patientICN admit_or_vis_datetime;
run;

* only keep people whose first lung ca dx was during the mar 2018 - feb 2022 study period ;
data firstlungdx;
set firstlungdx;
by patientICN admit_or_vis_datetime;
if first.patientICN then do;
	if datepart(admit_or_vis_datetime)>='01Mar2018'd then output;
end;
keep patientICN; 
run; */

* update 26 Jan 2023 -- now using Nabins 'finderfilea' dataset which has the following attrition 
flags already created:

step                            before      removed

flag00_alive_01mar18        10,893,614      892,913
flag01_age                  10,000,701    4,634,488
flag02_veteran               5,366,213      422,512
flag03_enrolled_1yrprior     4,943,701      678,607
flag04_enc_2yr               4,265,094      575,919
flag05_current_smok          3,689,175    2,858,571
flag06_nolungdxprior           830,604        8,845
flag07_lungdxindex_mar22       821,759      803,698
FINAL POP                       18,061            .


... to this, we will slightly modify to require 2+ years of enrollment rather than 1 ;

libname lc "&PROJROOT/Data/LC";

data firstlungdx;
set lc.finder_fileA;  * note this is patient level, not event level ;
IF min(of flag:) AND ('01Mar2022'd-first_enroll)>=730;
run;

** note -- incident cancer dx dates were found for the period 01Mar2018 through 31Aug2022 - 
corresponding radiology reports are pulled here for that period shifted 
backwards by 6 months, i.e., 01Sep2017 through 28Feb2022 (last start 01Feb) ;
%dateloop(firststart=01Sep2017, laststart=01Feb2022, interval=month, intmult=1, 
	dxeventfile=lc.finder_filea, dxeventdatevar=dx_date_CA);



/*

select stack.patientSID, stack.radiologynuclearmedicineReportSID, stack.reportText,
re.radiologydiagnosticCodeSID, rdx.RadiologyDiagnosticCode, 
rdx.RadiologyDiagnosticCodeDescription from
  (
  select patientSID, radiologynuclearmedicineReportSID, impressiontext as reportText
  FROM [ORD_Davies_202110040D].[Src].[SPatientText_RadiologyImpressions]
  union all
  select patientSID, radiologynuclearmedicineReportSID, reportText
  FROM [ORD_Davies_202110040D].[Src].[SPatientText_RadiologyReportText]) stack
  inner join
  (select patientSID, radiologynuclearmedicinereportSID, radiologydiagnosticCodeSID
  FROM [ORD_Davies_202110040D].[Src].[Rad_RadiologyExam]) re
  on stack.patientSID=re.patientSID 
  and 
 stack.radiologynuclearmedicineReportSID=re.radiologynuclearmedicineReportSID
 left join
 CDWWork.Dim.RadiologyDiagnosticCode rdx
 on re.RadiologyDiagnosticCodeSID=rdx.RadiologyDiagnosticCodeSID

*str(reportText like '%chest CT%' or reportText like '%chest x%' or reportText like '%CXR%' 
				or reportText like '%biopsy%' or reportText like '%bx%'
*/
