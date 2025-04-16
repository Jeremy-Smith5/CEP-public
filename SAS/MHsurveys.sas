/*
PROGRAM: MHsurveys.sas

Pull questions and responses for surveys located in the Mental Health (MH) domain
for a specified set of patients and date range.

vhawrjsmithj
Mar 2025

NOTES: 
	- user must define a libname called 'sdat' as follows:
		libname sdat sqlsvr datasrc=<ORD PROJECT> &sql_optimal schema=dflt;  
	- the search strings specified in the input dataset <SURVEYLIST> will be
		used to search the 'SurveyName' field in MH_SurveyAdministration - however,
		note that Dim.Survey also contains a field called PrintTitle.  If the 
		surveys are unfamiliar, suggest first manually searching 
		Dim.Survey using both SurveyName and PrintTitle to identify all
		unique SurveyNames that may be applicable.


EXAMPLE CALL:

libname esk "&PROJROOT/Data";
libname sdat sqlsvr datasrc=&PROJ &sql_optimal schema=dflt;

data pops;
set pops (keep=subpop patientICN endFUall indexdate);
length index_minus360 5;
format index_minus360 date9.;
index_minus360=indexdate-360;
keep subpop patientICN index_minus360 endFUall;
run;

data surveylist;
infile cards dsd truncover firstobs=1 dlm=',';
length surveyName $20;
input surveyName;
cards;
%ADL%
%WHO%DAS%
;
run;

%MHsurveys(
        ptlist=pops,                 
        startdatevar=index_minus360, 
        enddatevar=endFUall,         
        global_start=,               
        global_end=,                 
        surveylist=surveylist       
        );                          

data esk.disab_adl_surveys;
set MHsurveys;
by patientICN surveyName surveyadministrationSID designator;
run;

================================================================================= */

%MACRO MHsurveys(
	ptlist=,	/* SAS dataset containing (patientICN $10 and, optionally, <startdatevar> and <enddatevar> formatted as dates) */
	startdatevar=,  /* start date variable, if any, present in PTLIST */
	enddatevar=,    /* end date variable, if any, present in PTLIST -- if startdatevar is specified, then enddate must also be specified */
	global_start=,  /* e.g., 15Mar2021 - if startdatevar / enddatevar are NOT specified, user MUST specify a global date range for which to pull surveys */
	global_end=,  	/* e.g., 31Dec2024 -  if startdatevar / enddatevar are NOT specified, user MUST specify a global date range for which to pull surveys */
	surveylist=	/* SAS dataset containing (surveyName $) - a character search string with % operators as needed */
	); 		/* output: WORK.MHsurveys -- deduplicated by surveyAdministrationSID and designator (question) */

	%let svOK=0;
	%let evOK=0;
	%let pvOK=0;

	%if %length(&startdatevar) %then %do;

		proc contents data=&ptlist noprint out=ptvars (keep=name format); run;

		data _null_;
		set ptvars;
		name=lowcase(name);
		format=lowcase(format);
		if (index(format,'date') or index(format,'yymmdd')) and NOT index(format,'time') then do;
			if name=lowcase("&startdatevar") then call symputx("svOK",1);	
			if name=lowcase("&enddatevar") then call symputx("evOK",1);
		end;
		if name='patienticn' then call symputx("pvOK",1);
		run;

		%if &svOK=0 OR &evOK=0 OR &pvOK=0 %then %do;
			%put ::: ERROR: &ptlist must contain patientICN, &startdatevar and &enddatevar with dates formatted as dates;
			%abort cancel;
		%end;

		proc sql noprint;
		select min(&startdatevar) into :global_start trimmed from &ptlist;
		select max(&enddatevar) into :global_end trimmed from &ptlist;
		quit;

		data _null_;
		call symputx("gsSQL",catx('-',year(&global_start),put(month(&global_start),z2.),put(day(&global_start),z2.)));
		call symputx("geSQL",catx('-',year(&global_end),put(month(&global_end),z2.),put(day(&global_end),z2.)));
		run;

		%put ::: calculated GLOBAL START: &gsSQL ... calculated GLOBAL END: &geSQL;

	%end;
	%else %do;

		%if %length(&global_start)=0 OR %length(&global_end)=0 %then %do;
			%put ::: ERROR: you must either provide patient-specific start and end date variables or a global start/end range ;
			%abort cancel;
		%end;

		data _null_;
		call symputx("gsSQL",catx('-',year("&global_start"d),put(month("&global_start"d),z2.),put(day("&global_start"d),z2.)));
		call symputx("geSQL",catx('-',year("&global_end"d),put(month("&global_end"d),z2.),put(day("&global_end"d),z2.)));
		run;

		%put ::: using  GLOBAL START: &gsSQL ... GLOBAL END: &geSQL;
		
	%end;

	proc sql noprint;
	select (count(*)>count(distinct patientICN)) into :dupPts trimmed from &ptlist;
	quit;

	%if &dupPts %then %do;
		%put ::: WARNING: your input file &ptlist is not one row per patient - a deduplicated temp file has been created ;
		proc sort data=&ptlist out=dedup_pts NODUPKEY; by patientICN; run;
		%let ptlist=dedup_pts;
	%end;

	data sdat.'#ptlist'n (dbtype=(patientICN='varchar(10)'));
	set &ptlist (keep=patientICN %if &svOK %then &startdatevar &enddatevar;);
	run;

	data sdat.'#surveylist'n /*(dbtype=(surveyName='varchar(20)'))*/;
	set &surveylist (keep=surveyName);
	run;

	proc sql;
	connect to sqlsvr as sdat (datasrc=&PROJ &SQL_OPTIMAL);

	execute(select distinct a.patientICN, xw.patientSID %if &svOK %then , a.&startdatevar, a.&enddatevar; into #pts
		from
			#ptlist A
			inner join
			[&PROJ].[Src].[CohortCrosswalk] xw
			on a.patientICN=xw.patientICN) by sdat;

	* pulling survey answers and choice ;
	execute(select distinct coh.patienticn, %if &svOK %then coh.&startdatevar, coh.&enddatevar, ;
		coh.patientsid, sl.surveyName as search_string, mh.surveyname, mh.surveygivendatetime, 
		mh.sta3n, mh.surveyadministrationsid, 'surveyAdministration' as x_source_table, 
		mh.surveysid, mh.surveyadministrationsid as x_source_sid, mh.iscompleteflag, 
		mh.numberofquestionsanswered
		into #survey
		from 
			#pts COH
			INNER JOIN 
			(select patientsid, surveyname, surveygivendatetime, sta3n, surveyadministrationsid, surveysid, iscompleteflag, numberofquestionsanswered 
				from [&PROJ].[Src].[MH_SurveyAdministration] 
				WHERE
				surveyGivenDateTime>=cast(%nrbquote(')&gsSQL.T00:00:00.000%nrbquote(') as datetime2(0))
				and
				surveyGivenDateTime<=cast(%nrbquote(')&geSQL.T23:59:59.999%nrbquote(') as datetime2(0))) MH
			ON 
				coh.patientsid=mh.patientsid 
				%if &svOK %then %do;
					and
					cast(mh.surveyGivenDateTime as date)>=coh.&startdatevar
					and
					cast(mh.surveyGivenDateTime as date)<=coh.&enddatevar
				%end;
					
			INNER JOIN
			#surveylist SL
			on mh.surveyname LIKE sl.surveyname;
		) by sdat;

	create table MHsurveys as
	select 
		patientICN length=10, 
		%if &svOK %then %do;
			&startdatevar length=5 format=date9.,
			&enddatevar length=5 format=date9.,
		%end;
		search_string length=32,
		surveyName length=32, 
		sta3n length=6,
		datepart(surveyGivenDateTime) as surveydate length=5 format=date9.,
		surveyAdministrationSID,
		iscompleteflag length=3,
		numberofquestionsanswered length=3,
		input(designator,3.) as designator length=3,
		questionSequence length=5, 
		surveyQuestionText as question length=75,
		surveyChoiceText as choice length=75, 
		LegacyValue as char_score length=10
		from connection to SDAT (
			select 
			a.*, 
			sq.surveyQuestionText, 
			sc.questionSequence,   /* this is the order the questions are asked, but not necessarily the question number since questions can be asked out of sequence  */
			sc.designator,	/* this is really the question number */
			ch.surveyChoiceText,
			ch.LegacyValue,
			t.surveyanswertext,
			surv.printtitle,
			surv.purpose,
			surv.reference,
			surv.version
			from
				#survey A
				INNER JOIN
				[CDWWORK].[Dim].[Survey] as SURV
				on 
					a.surveysid=surv.surveysid
				INNER JOIN
				[CDWWork].[Dim].[SurveyContent] SC
				on 
					a.surveySID=sc.surveySID
				LEFT JOIN
				[&PROJ].[Src].[MH_SurveyAnswer] as ANS
				on 
					a.patientSID=ans.patientSID
					and
					a.sta3n=ans.sta3n
					and
					a.surveyadministrationsid=ans.surveyadministrationsid 
					and 
					sc.surveyquestionsid=ans.surveyquestionsid
				LEFT JOIN
				[CDWWORK].Dim.Surveyquestion as SQ
				on 
					sc.surveyquestionsid=sq.surveyquestionsid
				LEFT JOIN
				[CDWWork].[Dim].[SurveyChoice] CH
				on 
					ans.surveyChoiceSID=ch.surveyChoiceSID
				LEFT JOIN
				[&PROJ].[Src].[MH_SurveyAnswerText] as T
				on 
					ans.patientSID=t.patientSID
					and
					ans.sta3n=t.sta3n
					and
					ans.surveyanswersid=t.surveyanswersid
				)

	order by patientICN, surveyName, surveyadministrationSID, designator, char_score;

	execute (drop table if exists #survey) by sdat;
	execute (drop table if exists #pts) by sdat;
	execute (drop table if exists #ptlist) by sdat;

	disconnect from sdat;

	quit;

	* in case any duplicates exist of a given question (designator) for a given administration of the survey, preferentially
	keep the one with the higher/non-missing question-level score ;
	data MHsurveys;
	set MHsurveys;
	by patientICN surveyName surveyadministrationSID designator;
	IF last.designator; * e.g., if designator 7 appears twice, once with a missing score and once with a score of 3, keep the score of 3 for question 7 ;
	run;

%MEND; *MHsurveys();

