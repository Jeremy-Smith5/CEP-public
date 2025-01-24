/*
PROGRAM: pull.PHQ.questlev.sas

Pull PHQ-2, PHQ-2+I9 and PHQ-9 questions and responses.

Authors:
Jeremy Smith / Gabrielle Zwain

WRJ Vermont VAMC

15 Nov 2024

NOTE: this is a newly developed program - use with caution.

UPDATE: removed the field LegacyChoice / char_score - this seems to be from
an outdated scoring system.  Just leaving the text answer here instead.  
Refer to, e.g., 
https://med.stanford.edu/fastlab/research/imapp/msrs/_jcr_content/main/accordion/accordion_content3/download_256324296/file.res/PHQ9%20id%20date%2008.03.pdf
to translate choice text to a numerical score:

For questions 1-9:
  "Not at all"             =0
  "Several days"           =1
  "More than half the days"=2
  "Nearly every day"       =3

================================================================================= */


%let PROJ=ord_smith_202410066d;  * project database ;

libname anh "/data/dart/2024/&PROJ/Data";

%let outlib=ANH;  * libname for output data to be saved ;
%let cohort_dataset=WORK.pops;  * cohort of patients to pull PHQ data for -- should be 1 row per patient and contain patientICN ;

libname sdat sqlsvr datasrc=&PROJ &sql_optimal schema=dflt;

* date range for pulling PHQ data ;
%let startdate=2017-01-01; * inclusive ;
%let enddate=2023-01-01; * exclusive ;

data sdat.'#fullpop'n (dbtype=(patientICN='varchar(10)'));
set &cohort_dataset (keep=patientICN);
run;

proc sql;
connect to sqlsvr as sdat (datasrc=&PROJ &SQL_OPTIMAL);

execute(select distinct a.patientICN, xw.patientSID into #ptlist
	from
		#fullpop A
		inner join
		[&PROJ].[Src].[CohortCrosswalk] xw
		on a.patientICN=xw.patientICN) by sdat;

* pulling survey answers and choice ;
execute(select distinct coh.patienticn, coh.patientsid, mh.surveyname, mh.surveygivendatetime, mh.sta3n, mh.surveyadministrationsid, 
		'surveyAdministration' as x_source_table, mh.surveysid, mh.surveyadministrationsid as x_source_sid, mh.iscompleteflag, mh.numberofquestionsanswered
		into #survey
	from #ptlist as COH
	inner join (select patientsid, surveyname, surveygivendatetime, sta3n, surveyadministrationsid, surveysid, iscompleteflag, numberofquestionsanswered 
			from [&PROJ].[Src].[MH_SurveyAdministration] 
			where (surveyName like 'PHQ%2%' OR surveyName like 'PHQ%9%') 
			and 
			surveyGivenDateTime>=cast(%nrbquote(')&startdate.T00:00:00.000%nrbquote(') as datetime2(0))
			and
			surveyGivenDateTime<cast(%nrbquote(')&enddate.T00:00:00.000%nrbquote(') as datetime2(0))
			 )as MH
	on coh.patientsid = mh.patientsid
	) by sdat;

create table survey_textraw as
select 
	patientICN length=10, 
	surveyName length=10, 
	sta3n length=6,
	datepart(surveyGivenDateTime) as surveydate length=5 format=date9.,
	surveyAdministrationSID,
	iscompleteflag length=3,
	numberofquestionsanswered length=3,
	input(designator,3.) as designator length=3,
	questionSequence length=5, 
	surveyQuestionText as question length=75,
	surveyChoiceText as choice length=75
 	/*, 
	LegacyValue as char_score length=10  -- removed -- see note in header */
	from connection to SDAT (
		select 
		a.*, 
		b.surveyQuestionText, 
		c.questionSequence,   /* this is the order the questions are asked, but not necessarily the question number since questions can be asked out of sequence  */
		c.designator,	/* this is really the question number */
		d.surveyChoiceText,
		d.LegacyValue,
		t.surveyanswertext,
		surv.printtitle,
		surv.purpose,
		surv.reference,
		surv.version
		from
			#survey A
			inner join
			[CDWWORK].[Dim].[Survey] as surv
			on a.surveysid = surv.surveysid
			inner join
			[CDWWork].[Dim].[SurveyContent] C
			on a.surveySID=c.surveySID
			left join
			[&PROJ].[Src].[MH_SurveyAnswer] as ans
			on A.surveyadministrationsid = ans.surveyadministrationsid and C.surveyquestionsid = ans.surveyquestionsid
			and 
			ans.surveyGivenDateTime>=cast(%nrbquote(')&startdate.T00:00:00.000%nrbquote(') as datetime2(0))
			and
			ans.surveyGivenDateTime<cast(%nrbquote(')&enddate.T00:00:00.000%nrbquote(') as datetime2(0))
			LEFT JOIN
			[CDWWORK].Dim.Surveyquestion as B
			on c.surveyquestionsid = b.surveyquestionsid
			left join
			[CDWWork].[Dim].[SurveyChoice] D
			on ans.surveyChoiceSID=d.surveyChoiceSID
			LEFT JOIN
			[&PROJ].[Src].[MH_SurveyAnswerText] as T
			on ans.surveyanswersid = t.surveyanswersid
			and 
			t.surveyGivenDateTime>=cast(%nrbquote(')&startdate.T00:00:00.000%nrbquote(') as datetime2(0))
			and
			t.surveyGivenDateTime<cast(%nrbquote(')&enddate.T00:00:00.000%nrbquote(') as datetime2(0))
			)
order by patientICN, surveyName, surveyadministrationSID, designator, char_score;

execute (drop table if exists #fullpop) by sdat;
execute (drop table if exists #survey) by sdat;
execute (drop table if exists #ptlist) by sdat;

disconnect from sdat;

quit;

* in case any duplicates exist of a given question (designator) for a given administration of the survey, preferentially
keep the one with the higher/non-missing question-level score ;
data &outlib..phq_questionlevel;
set survey_textraw;
by patientICN surveyName surveyadministrationSID designator;
IF last.designator; * e.g., if designator 7 appears twice, once with a missing score and once with a score of 3, keep the score of 3 for question 7 ;
run;


