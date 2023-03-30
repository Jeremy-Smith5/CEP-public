
/*
PROGRAM: pull.TIU_text.sas

Relatively crude first pass through ReportText field in STIUNotes_TIU_8925 view - 
first subsetting to a given month, then to anything containing 'lung', then 
looking for more cancer-specific things, then appending to stack of 
previous months, then saving a permanent SAS dataset.

vhawrjsmithj
14 Nov 2022

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

%MACRO pull_TIU(
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
				(select patientSID from [&PROJ].[src].[STIUNotes_TIUDocument_8925]
				WHERE 
					episodebegindatetime>=@startdt
					and
					episodebegindatetime< @enddt) B
				on a.patientSID=b.patientSID) by sdat;

		select nrecs into :totrecs from connection to sdat(select nrecs from #lungTS);

		%put ::: There were &totrecs STIUNotes records for the time period &dtstart - &dtend ;

		endsas;

	%end;

	%else %do;

		* pull data ;

		execute(
			declare @startdt datetime2(0);
			declare @enddt datetime2(0);

			set @startdt=cast(%nrbquote(')&dtstart.T00:00:00.000%nrbquote(') as datetime2(0));
			set @enddt=cast(%nrbquote(')&dtend.T00:00:00.000%nrbquote(') as datetime2(0));

			select a.patientICN, b.patientSID, b.tiudocumentSID, b.parentTIUdocumentSID, b.episodebegindatetime, b.visitSID,
			b.TIUdocumentdefinitionSID, c.TIUdocumentdefinition, b.reportText
			into #lungTS
			from 
				#ptx A
				inner join
				(select patientSID, tiudocumentSID, parentTIUdocumentSID, episodebegindatetime, visitSID, 
				TIUdocumentdefinitionSID, reportText
				from [&PROJ].[src].[STIUNotes_TIUDocument_8925]
				WHERE 
					episodebegindatetime>=@startdt
					and
					episodebegindatetime< @enddt) B
				on a.patientSID=b.patientSID
				inner join
				(select distinct TIUdocumentdefinitionSID, TIUdocumentdefinition 
				from [CDWWork].[Dim].[TIUDocumentDefinition] &TIUdocdef) C
				on b.TIUdocumentdefinitionSID=c.TIUdocumentdefinitionSID
				) by sdat;

		execute(
			select patientICN, patientSID, tiudocumentSID, parentTIUdocumentSID, episodebegindatetime, visitSID, 
			TIUdocumentdefinitionSID, TIUdocumentdefinition, reportText
			into #tiuW1
			from #lungTS
			WHERE
				&WHERE1
				) by sdat;

		execute(drop table if exists #lungTS) by sdat;

		%let tempname=#tiuW1;

		%if %length(&where2) %then %do;

			execute(
				select patientICN, patientSID, tiudocumentSID, parentTIUdocumentSID, episodebegindatetime, visitSID, 
				TIUdocumentdefinitionSID, TIUdocumentdefinition, reportText
				into #tiuW2
				from #tiuW1
				WHERE
					&WHERE2
					) by sdat;

			execute(drop table if exists #tiuW1) by sdat;

			%let tempname=#tiuW2;

			%if %length(&where3) %then %do;
		
				execute(
					select patientICN, patientSID, tiudocumentSID, parentTIUdocumentSID, episodebegindatetime, visitSID, 
					TIUdocumentdefinitionSID, TIUdocumentdefinition, reportText
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
		select patientICN length=10, patientSID, 
		tiudocumentSID, parentTIUdocumentSID, episodebegindatetime, visitSID, 
		TIUdocumentdefinitionSID, TIUdocumentdefinition length=60, reportText length=16384
		from connection to sdat (
			select distinct patientICN, patientSID, tiudocumentSID, 
			parentTIUdocumentSID, episodebegindatetime, visitSID, 
			TIUdocumentdefinitionSID, TIUdocumentdefinition, reportText
			from &tempname);

		execute(drop table if exists &tempname) by sdat;

	%end; * pull data ;

	disconnect from sdat;

	quit;

%MEND; *pull_TIU();

%MACRO dateloop(firststart=, laststart=, interval=month, intmult=1);
	
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

		%pull_TIU(
			dtstart=&currStartSQL, /* inclusive */
			dtend=&currEndSQL,   /* exclusive */
			ptlist=firstlungdx,
			check_n=0,
			outdata=tiumonth,
			where1=%str(reportText like '%lung%'), 
			where2=%str(reportText like '%tumor%' or reportText like '%nodule%' or reportText like '%mass%' 
				or reportText like '%lesion%' or reportText like '%neoplasm%' or reportText like '%carcinoma%'),
			where3=%str(reportText like '%chest CT%' or reportText like '%chest x%' or reportText like '%CXR%' 
				or reportText like '%biopsy%' or reportText like '%bx%')
			);

		data tiumonth;
		length wnum 3 wstart wend 4;
		set tiumonth end=last;
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

		proc append data=tiumonth base=all_tiumonth; run;

		proc sql;
		drop table tiumonth;
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

	data feas.tiu_raw_pull_&firststart._&lastend;
	set all_tiumonth;
	run;

	proc sql;
	drop table all_tiumonth;
	run;

%MEND; *dateloop();

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
run;

%dateloop(firststart=01Mar2018, laststart=01Feb2022, interval=month, intmult=1);


