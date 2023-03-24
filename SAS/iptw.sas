/*
This program provides a brief demo of inverse propensity of treatment weighting (IPTW)
by first creating a simulated patient-level dataset with indicators for a 'real_world_drug'
(with likelihood of receipt influenced by demographic factors) and a 'trial_drug' (with 
likelihood of receipt being random) along with a corresponding real world outcome and 
trial outcome.  After that, models are run to compare results of the influence of the 
trial drug on the trial outcome and the influence of the real world drug on the real
world outcome (unweighted and IPTW, separately) -- see first three PHREG steps.  Weighted 
and unweighted table 1s are constructed as well.

This is a quick demo only and should not be construed as the work of an expert on 
this topic! 

Jeremy Smith
May 2022

NOTE: we could also incorporate inverse probability of censoring weights here.

=========================================================================================== */

options nocenter nonumber nodate mprint mergeNoBy=error formdlim=' ' pagesize=max linesize=180;

* create a simulated dataset ;
data pop;
call streaminit(618810345);

do pt=1 to 10000;
	female=(rand('uniform')<0.6);

	array r {10} $1 _temporary_ ('B','B','B','W','W','W','W','W','W','O');
	race=r[int(rand('uniform')*dim(r))+1];  

	income=0;
	comorbs=-1;

	do i=1 to 3;

		* make age partially a function of sex and race ;
		age=30 + female * 7.25 + (race='B') * -4.28 + (race='O') * 6.15 
			+ (0<income<20000) * -9.2 + (20000<=income<29000) * -4.0 + (income>=38000) * -1.8
			+ (0<comorbs<3) * -2.9 + (comorbs=3) * 0.82 + (comorbs>=6) * 2.5
			+ rand('erlang',2) * 10;

		* make income partially a function of age, sex and race ;
		income=25000 + female * -4000 + (race='B') * -8500 + (race='O') * 4500 
			+ (age<40) * -6000 + (40<=age<48) * -1700 + (age>=57) * -3000
			+ (0<=comorbs<3) * 11500 + (comorbs=3) * 2600 + (comorbs>=6) * -8200
			+ rand('erlang',2)*7500;

		* make comorbidity count partially a function of age, sex, race and income ;
		comorbs=max(0, int(2 + female * -0.15 + (race='B') * 1.45 + (race='O') * -0.21 
			+ (age<40) * -2.2 + (40<=age<48) * -1.6 + (age>57) * 4.2
			+ (income<20000) * 2.5 + (20000<=income<29000) * 1.3 + (income>38000) * 0.65
			+ rand('erlang',1)*1.55));

	end;

	* randomize people to a clinical trial drug ;
	trial_drug = (rand('uniform')<0.333);

	* make access to the real-world drug partially a function of age, sex, race, income and comorbidity count ;
	real_world_drug = (logistic(0.25 + female * 1.25 + (race='B') * -1.9 + (race='O') * -0.88 
		+ (age<40) * -1.85 + (40<=age<48) * -0.41 + (age>57) * 2.8
		+ (income<20000) * -1.36 + (20000<=income<29000) * -1.66 + (income>38000) * 0.70
		+ (comorbs<3) * 1.9 + (comorbs=3) * -0.25 + (comorbs>=6) * -1.1
		+ rand('erlang',1)*0.57)<0.52075);

	* make real world outcome (MI) partially a function of age, sex, race, income, comorbidity count and receipt of real world drug ;
	real_world_outcome = (logistic(0.2 + female * 0.92 + (race='B') * -1.4 + (race='O') * 0.77 
		+ (age<40) * -0.92 + (40<=age<48) * -0.45 + (age>57) * 0.82
		+ (income<20000) * -0.39 + (20000<=income<29000) * 0.89 + (income>=38000) * -1.08
		+ (comorbs<3) * -1.05 + (comorbs=3) * -0.19 + (comorbs>=6) * 1.23
		+ real_world_drug * -0.45
		+ rand('erlang',1)*-0.21)<0.37578);

	* make trial outcome (MI) partially a function of age, sex, race, income, comorbidity count and receipt of trial drug ;
	* note all coefficients are the same as above - the only difference is access to the drug through randomization vs. real world ;
	trial_outcome = (logistic(0.2 + female * 0.92 + (race='B') * -1.4 + (race='O') * 0.77 
		+ (age<40) * -0.92 + (40<=age<48) * -0.45 + (age>57) * 0.82
		+ (income<20000) * -0.39 + (20000<=income<29000) * 0.89 + (income>=38000) * -1.08
		+ (comorbs<3) * -1.05 + (comorbs=3) * -0.19 + (comorbs>=6) * 1.23
		+ trial_drug * -0.45
		+ rand('erlang',1)*-0.21)<0.37578);

	if real_world_outcome then real_world_days=int(rand('uniform')*180+1);
	else real_world_days=min(180, int(rand('uniform')*360+1));
	
	if trial_outcome then trial_days=int(rand('uniform')*180+1);
	else trial_days=min(180, int(rand('uniform')*360+1));

	output;
end;
drop i;
run;

title "first 20 obs";
proc print data=pop (obs=20) noobs heading=v width=min; run;
title;

/*
proc means data=pop min mean median p99 max;
var age income comorbs real_world_drug real_world_outcome;
run; */

/*
proc univariate data=pop noprint;
var real_world_outcome;
output out=chkrwd pctlpre=p pctlpts=40;
run;

proc print data=chkrwd; run;
endsas; */

proc means data=pop noprint;
var age income comorbs;
output out=qs p25= median= p75=/autoname;
run;

/*
proc print data=qs heading=v width=min; run; */

data _null_;
set qs;
call symputx("ageQ1",age_p25);
call symputx("ageQ2",age_median);
call symputx("ageQ3",age_p75);

call symputx("incomeQ1",income_p25);
call symputx("incomeQ2",income_median);
call symputx("incomeQ3",income_p75);

call symputx("comorbsQ1",comorbs_p25);
call symputx("comorbsQ2",comorbs_median);
call symputx("comorbsQ3",comorbs_p75);
run;


proc format;
value fdr
	0='no_Drug'
	1='Drug'
	;
value fage
	low-<&ageQ1='ageQ1'
	&ageQ1-<&ageQ2='ageQ2'
	&ageQ2-<&ageQ3='ageQ3'
	&ageQ3-high='ageQ4'
	;

value finc
	low-<&incomeQ1='incQ1'
	&incomeQ1-<&incomeQ2='incQ2'
	&incomeQ2-<&incomeQ3='incQ3'
	&incomeQ3-high='incQ4'
	;

value fcom
	low-<&comorbsQ1='comQ1'
	&comorbsQ1-<&comorbsQ2='comQ2'
	&comorbsQ2-<&comorbsQ3='comQ3'
	&comorbsQ3-high='comQ4'
	;
run;


ods listing close;

	* calculate each persons probability of receiving the real world drug based on
	all two-way interactions between age, race, sex, income and comorbidities ;
	proc logistic data=pop DESCENDING;
	format age fage. income finc. comorbs fcom.;
	class age (ref='ageQ2') race (ref='W') income (ref='incQ3') comorbs (ref='comQ1');
	model real_world_drug = age | race | female | income | comorbs @2;
	output out=preds predicted=ps;
	run;

ods listing;

* create weights from the predicted values generated by the model above ;
proc sql noprint;
select mean(real_world_drug) into :pR from preds;
quit;

data preds;
set preds (keep=pt real_world_drug ps);
if real_world_drug then do;
	stab_IPTW=&pR/ps;
	IPTW=1/ps;
end;
else do;
	stab_IPTW=(1-&pR)/(1-ps);
	IPTW=1/(1-ps);
end;
run;

proc means data=preds noprint;
class real_world_drug;
var stab_iptw;
output out=pcts p99=/autoname;
run;

data _null_;
set pcts;
if real_world_drug=0 then call symputx("rwd0",stab_IPTW_p99);
if real_world_drug=1 then call symputx("rwd1",stab_IPTW_p99);
run;

/*
proc univariate data=preds noprint;
var stab_iptw;
output out=p1p99_stabwts pctlpts=1 99 pctlpre=sw pctlname=p1 p99;
run; */

/*
proc means data=preds min p1 mean median p99 max;
class real_world_drug;
var stab_IPTW IPTW;
run; */

/*
proc univariate data=preds noprint;
var stab_iptw;
output out=p1p99_stabwts pctlpts=1 99 pctlpre=sw pctlname=p1 p99;
run; */

* join the stabilized IPTW back with the original file ;
proc sql;
create table pop as
select a.*, b.stab_IPTW
from
	pop A
	left join
	preds B
	on a.pt=b.pt
order by a.pt;
quit;

* replicate people according to their IPTW -- note this is only being done for 
purposes of the table 1 - not the models that follow.  If using proc means, 
proc report or similar, we could just use the WEIGHT statement rather than 
(crudely) replicating people here ;
data wpop;
set pop;
* if weight is greater than 99th percentile, set weight to twice the 99th percentile ;
/*
if real_world_drug=0 then do;
	if stab_iptw>&rwd0 then stab_iptw=stab_iptw*2;
end;
else do;
	if stab_iptw>&rwd1 then stab_iptw=stab_iptw*2;
end; */
do i=1 to round(stab_iptw * 10);
	output;
end;
run;

%include '/data/prod/common/WRJ_macros/table1.sas';

* unweighted table 1 stratified by receipt of trial drug (i.e., cohort) ;
%table1(
	personfile=pop,	/* name of person-level input file containing STRATVARS and ROWVARS */
	stratvars=trial_drug:fdr,		/* pipe-separated list of stratifying variables as described above */
	rowvars=
		female |
		race |
		age:fage |
		income:finc |
		comorbs:fcom
		,
	uselabels=0,	/* if set to 1 and variable label on input dataset (<personfile>) is <=32 characters, use var label instead of name */
	usecase=0,	/* if set to 1, use case of variable name as provided in macro call (unless using label) */
	pvalues=0,	/* 1: will perform an M-H chi-square test for categorical variables, a T-test for 2-way means, KW for 2-way medians */
	printSMD=1
	);

data table1_trial (rename=(no_Drug_2=no_Drug Drug_2=Drug));
set table1 (keep=var lvl no_Drug_2 Drug_2 SMD);
if var='Pop' then SMD=.;
format no_Drug_2 Drug_2 percent8.1;

* absolute value of standardized mean difference (produced by table1 macro) ;
asmd=abs(SMD);
run;

proc sql noprint;
select mean(asmd) into :asmd from table1_trial;
quit;

data table1_trial;
set table1_trial (drop=asmd);
if var='Pop' then avg_abs_SMD=&asmd;
run;

title "trial drug";
proc print data=table1_trial noobs width=min; run;

proc sql; drop table table1; quit;

* unweighted table 1 stratified by receipt of real world drug ;
%table1(
	personfile=pop,	/* name of person-level input file containing STRATVARS and ROWVARS */
	stratvars=real_world_drug:fdr,		/* pipe-separated list of stratifying variables as described above */
	rowvars=
		female |
		race |
		age:fage |
		income:finc |
		comorbs:fcom
		,
	uselabels=0,	/* if set to 1 and variable label on input dataset (<personfile>) is <=32 characters, use var label instead of name */
	usecase=0,	/* if set to 1, use case of variable name as provided in macro call (unless using label) */
	pvalues=0,	/* 1: will perform an M-H chi-square test for categorical variables, a T-test for 2-way means, KW for 2-way medians */
	printSMD=1
	);

data table1_rwd (rename=(no_Drug_2=no_Drug Drug_2=Drug));
set table1 (keep=var lvl no_Drug_2 Drug_2 SMD);
if var='Pop' then SMD=.;
format no_Drug_2 Drug_2 percent8.1;

asmd=abs(SMD);
run;

proc sql noprint;
select mean(asmd) into :asmd from table1_rwd;
quit;

data table1_rwd;
set table1_rwd (drop=asmd);
if var='Pop' then avg_abs_SMD=&asmd;
run;

title "real world drug -- unweighted";
proc print data=table1_rwd noobs width=min; run;

proc sql; drop table table1; quit;

* IPT-weighted table 1 stratified by receipt of real world drug ;
%table1(
	personfile=wpop,	/* name of person-level input file containing STRATVARS and ROWVARS */
	stratvars=real_world_drug:fdr,		/* pipe-separated list of stratifying variables as described above */
	rowvars=
		female |
		race |
		age:fage |
		income:finc |
		comorbs:fcom
		,
	uselabels=0,	/* if set to 1 and variable label on input dataset (<personfile>) is <=32 characters, use var label instead of name */
	usecase=0,	/* if set to 1, use case of variable name as provided in macro call (unless using label) */
	pvalues=0,	/* 1: will perform an M-H chi-square test for categorical variables, a T-test for 2-way means, KW for 2-way medians */
	printSMD=1
	);

data table1_rwd_wt (rename=(no_Drug_2=no_Drug Drug_2=Drug));
set table1 (keep=var lvl no_Drug_2 Drug_2 SMD);
if var='Pop' then SMD=.;
format no_Drug_2 Drug_2 percent8.1;

asmd=abs(SMD);
run;

proc sql noprint;
select mean(asmd) into :asmd from table1_rwd_wt;
quit;

data table1_rwd_wt;
set table1_rwd_wt (drop=asmd);
if var='Pop' then avg_abs_SMD=&asmd;
run;

title "real world drug -- weighted";
proc print data=table1_rwd_wt noobs width=min; run;

proc sql; drop table table1; quit;

proc export data=table1_trial dbms=xlsx replace
	outfile="/u/vhawrjsmithj/fake_data_iptw_demo.xlsx";
sheet="trial drug";
run;

proc export data=table1_rwd dbms=xlsx replace
	outfile="/u/vhawrjsmithj/fake_data_iptw_demo.xlsx";
sheet="real world drug unwt";
run;

proc export data=table1_rwd_wt dbms=xlsx replace
	outfile="/u/vhawrjsmithj/fake_data_iptw_demo.xlsx";
sheet="real world drug wt";
run;

** MODELS -- trial drug (unweighted), real-world drug (unweighted), real-world drug (IPTW) ;

ods select ParameterEstimates;
title "clinical trial";
proc phreg data=pop;
model trial_days * trial_outcome(0) = trial_drug / risklimits ties=efron;
run;

ods select ParameterEstimates;
title "observational study - unweighted";
proc phreg data=pop;
model real_world_days * real_world_outcome(0) = real_world_drug / risklimits ties=efron;
run;

ods select ParameterEstimates;
title "observational study - WEIGHTED";
proc phreg data=pop;
WEIGHT stab_iptw;
model real_world_days * real_world_outcome(0) = real_world_drug / risklimits ties=efron;
run;

ods select all;

* garbage below here ;

*endsas;

/*
title "clinical trial - no adjustment";
proc logistic data=pop DESCENDING;
format age fage. income finc. comorbs fcom.;
class age (ref='ageQ2') race (ref='W') income (ref='incQ3') comorbs (ref='comQ1');
model trial_outcome = trial_drug;
run;

title "clinical trial - ADJUSTED";
proc logistic data=pop DESCENDING;
format age fage. income finc. comorbs fcom.;
class age (ref='ageQ2') race (ref='W') income (ref='incQ3') comorbs (ref='comQ1');
model trial_outcome = trial_drug female race age income comorbs;
run;  */

