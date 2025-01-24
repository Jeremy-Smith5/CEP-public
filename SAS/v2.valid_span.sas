/*

SPAN: 
	Check 1+ inpatient / 2+ outpatient dx dates within a specified range for a given set of patients, conditions and diagnosis dates.
	

	NOTES:
		- The inputs PTFILE, CONDFILE and DXFILE can technically all point to the same file as long as that file is
			structured as described below for DXFILE.  However, note that output from this method will be
			limited to just those patients and conditions that are present in DXFILE.  Use separate input
			files to ensure that all patients and conditions are represented in the output.

		- For more options, use the original macro contained in valid_span.sas

		- Simulated input data and a sample call is shown below this macro.

		- This macro has not yet been tested on large data.
	
	vhawrjsmithj
	24 May 2024

	============================================================================================================================== */
	
%MACRO span(
	ptfile=,	/* file containing all unique patients -- contains: <PTID> */
	ptid=patientICN,/* patient identifier variable */
	condfile=,	/* file containing all unique conditions -- contains: condition */
	dxfile=,	/* file containing all dx events -- contains:  <PTID>, condition, dxdate, IP  -- IP is 0/1 indicator for inpatient */
	minspan=7,	/* min # of days required between dx dates in order to meet criteria */
	maxspan=180,	/* max # of days allowed between dx dates in order to meet criteria */
	IPonly1=1	/* 0/1 -- 0: inpatient is treated same as outpatient and requires 2+ records within span - 1: inpatient requires only 1 record */
	); /* output: WORK.ptlev_conditions */

	%let ptfile=%upcase(&ptfile);
	%let ptfilelib=WORK;
	%if %index(&ptfile,.) %then %do;
		%let ptfilelib=%scan(&ptfile,1,.);
		%let ptfile=%scan(&ptfile,2,.);
	%end;

	proc sql undo_policy=NONE noprint;
	select case when type='char' then compress('$' || put(length,3.)) else put(length,3.) end into :ptidlen trimmed 
	from dictionary.columns where libname="&ptfilelib" and upcase(memname)="&ptfile"
	and upcase(name)=upcase("&ptid");

	%if &IPonly1 %then %do;
		* duplicate inpatient records and give 2nd record a synthetic date that is <MINSPAN> days after original record ;
		create table dx as
		select &ptid, condition, dxdate from &dxfile
		union all
		select &ptid, condition, dxdate+&minspan from &dxfile WHERE IP=1;

		%let dxfile=WORK.dx;
	%end;

	create table ptlev_conditions as
	select ptcond.&ptid, ptcond.condition, anydx.first_overall, metcrit.first_qualifying 
	from
		(select * from 
			(select distinct &ptid from &ptfilelib..&ptfile) PTLIST
			CROSS JOIN
			(select distinct condition from &condfile) CONDLIST
		)PTCOND
		LEFT JOIN
		(select &ptid, condition, min(dxdate) as first_overall length=5 format=date9. 
		from &dxfile 
		group by &ptid, condition) ANYDX
		on 
			ptcond.&ptid=anydx.&ptid
			and
			upcase(ptcond.condition)=upcase(anydx.condition)
		LEFT JOIN
		(select L.&ptid, L.condition, min(L.dxdate) as first_qualifying length=5 format=date9.
		from
			&dxfile L
			INNER join
			&dxfile R
			on 
				L.&ptid=R.&ptid
				and
				upcase(L.condition)=upcase(R.condition)
				and
				&minspan<=R.dxdate-L.dxdate<=&maxspan
		group by L.&ptid, L.condition) METCRIT
		on 
			PTCOND.&ptid=METCRIT.&ptid
			and 
			upcase(PTCOND.condition)=upcase(METCRIT.condition)
	order by ptcond.&ptid, ptcond.condition;

	select count(distinct condition) into :ncond from &condfile;
	quit;

	proc sort data=&condfile NODUPKEY out=condlist; by condition; run;

	data cond2num (keep=fmtname type start label);
	set condlist end=last;
	length fmtname $8 type $1 start $32 label v 3;
	retain fmtname 'fcondnum' type 'C' v 1;
	array C1 {&ncond} $27 _temporary_;
	array C2 {&ncond} $27 _temporary_;
	array vorder {%eval(&ncond*2)} $27 _temporary_;
	start=condition;
	label=_N_;
	C1[_N_]=compress('any_' || condition);
	C2[_N_]=compress('conf_' || condition);
	vorder[v]=C1[_N_];
	vorder[v+1]=C2[_N_];
	v+2;
	if last then do;
		call symputx("anycond",catx(' ', of C1[*]));
		call symputx("confcond",catx(' ', of C2[*]));
		call symputx("varlist",catx(' ', of vorder[*]));
	end;
	run;

	proc format cntlin=cond2num; run;

	* note: the INPUT below is one row per person per condition - output (ptlev_conditions) is one row per person ;

	data 
		WORK.ptlev_conditions (drop=totalpop vartype)
		WORK.summary_conditions (keep=totalpop vartype any_:)
		;
	length &ptid &ptidlen totalpop 8 vartype $10 &varlist 5;
	set WORK.ptlev_conditions end=last;
	by &ptid;
	format &varlist date9.;
	array anyC {*} &anycond;
	array confC {*} &confcond;
	array anyT {&ncond} _temporary_;
	array confT {&ncond} _temporary_;
	retain totalpop 0 anyC confC;
	if first.&ptid then call missing(of anyC[*], of confC[*]);
	cnum=put(condition,$fcondnum.)*1;
	anyC[cnum]=first_overall;
	confC[cnum]=first_qualifying;
	anyT[cnum]+(first_overall>.);
	confT[cnum]+(first_qualifying>.);
	if last.&ptid then do;
		totalpop+1;
		output WORK.ptlev_conditions;
	end;
	if last then do;
		vartype='ANY';
		do i=1 to dim(anyC);
			anyC[i]=anyT[i]/totalpop;
		end;
		output WORK.summary_conditions;
		vartype='CONFIRMED';
		do i=1 to dim(confC);
			anyC[i]=confT[i]/totalpop;
		end;
		output WORK.summary_conditions;
	end;
	keep totalpop vartype &ptid &anycond &confcond;
	run;

	title "summary of overall and confirmed conditions";
	proc print data=WORK.summary_conditions noobs heading=v width=min;
	format any_: percent8.1;
	run;
	title;

%MEND; *span();

* sample call ;
/*
data pts;
length patientICN $10;
do i=1 to 100;
        patientICN=put(1000000000+i,10.);
        output;
end;
drop i;
run;

data conditions;
infile cards dsd truncover firstobs=1 dlm=',';
length condition $5;
input condition;
cards;
DM
CHF
HTN
CVD
COPD
PVD
;
run;

data dx;
set pts;
length condition $5 dxdate 5 IP 3;
format dxdate date9.;
array c {6} $5 _temporary_ ('DM', 'CHF', 'HTN', 'CVD', 'COPD', 'PVD');
array f {6} _temporary_ (0.25, 0.1, 0.45, 0.08, 0.19, 0.15);
do i=1 to dim(c);
        condition=c[i];
        dxdate='01Jan2020'd;
        do while (1);
                if ranuni(0)<f[i] then do;
                        dxdate=dxdate+int(ranuni(0)*500);
                        IP=(ranuni(0)<0.15);
                        output;
                end;
                if ranuni(0)**2>ranuni(0)*f[i] then leave;
        end;
end;
drop i;
run;

%span(
	ptfile=pts,
	ptid=patientICN,
	condfile=conditions,
	dxfile=dx,
	minspan=7,
	maxspan=180,
	IPonly1=1
	);

proc print data=WORK.ptlev_conditions (obs=50) noobs heading=v width=min; run; */









