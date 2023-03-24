
/*

	Perform exact matching on two provided datasets <SUB_CASES> and <SUB_CONTROLS> on
	the provided variable list <MATCHVARS>.  See example call below, which is abstracted
	from /data/dart/2020/ord_youngxu_202012009d/Programs/post_covid_cx/phase1.mk.matched.pop.sas

	Note that the macro 'minGap' is an optional user-provided snippet which is then called by
	the %match macro further down - this step can be anything required as long as it outputs
	a dataset called WORK.matched.

	UPDATE 06 Dec 2022 - match variables can now be specified to match within a given 
	range +/- or by means of a user-provided macro snippet (in addition to the original
	options of exact matching or format-based matching.  

	For exact matching, just specify the variable name.
	For format-based matching, separate variable name from format with a : (no period needed)
	For range matching, separate variable from number with : (by default, this will be assumed 
		as <= the given number.  To specify less than, just use < before number).
	For macro-based matching, separate variable name from macro snippet name with :  -- do NOT
		include a % sign - instead, use a ! sign, e.g., !state_match to run a macro called %state_match

	IN THE EXAMPLE BELOW, the matchvars parameter is set to match as follows:
		- age -- using age groups in a user-defined format called fage
		- exactly on the 0/1 variable male
		- exactly on the 0/1 variable for antivirals
		- within +/- 180 days of vaccine date
		- on HHS region using a user-defined macro snippet called hhsjoin like this:
			%macro hhsjoin;
				a.HHS=b.HHS OR a.HHS='??'
			%mend;

			-- NOTE: parentheses will be automatically added around this section

		
		,matchvars=age:fage male antvir vaxdate:180 HHS:!hhsjoin 

		The following ON clause will be built based on the above:

			ON
				put(a.age,fage.)=put(b.age,fage.)
				AND
				a.male=b.male
				AND
				a.antvir=b.antvir
				AND
				(a.HHS=b.HHS OR a.HHS='??')
				


	J Smith
	Sep 2022

	EXAMPLE CALL:

	%include "&CWD/match.sas";  * contains assign_ptnum() and match() ;

	%macro minGap;

		* snippet being run by match macro to minimize gap in lab dates ;
		proc sql;
		create table matchnew as
		select a.patientICN, a.cntrl_pt, b.labdate
		from
			matched A
			inner join
			(select distinct patientICN, labdate from t2sub) B
			on a.patientICN=b.patientICN;

		create table matchnew (drop=labdate) as
		select a.*, abs(a.labdate-b.cntrl_labdate) as labgap
		from
			matchnew A
			inner join
			(select distinct ptnum, cntrl_labdate from scontrols) B
			on a.cntrl_pt=b.ptnum
		order by a.patientICN, labgap;

		drop table matched;
		quit;

		proc datasets lib=work memtype=data nolist nodetails; change matchnew=matched; run; quit;

	%mend; *minGap;

	%assign_ptnum(
		 indata=t2
		,othkeepvars=age male antvir &emvlist conf_:
		,ptvar=patientICN
		,ptvarlen=$10
		);

	%match(
		 sub_cases=scases
		,sub_controls=scontrols
		,matchvars=age:fage male antvir vaxdate:180 HHS:!hhsjoin &extra_match_vars
		,lbl=&oc2
		,cntrls_per_case=4
		,w_replace=1
		,ptvar=patientICN
		,match_snip=minGap
		);

		* outputs a dataset called WORK.MATCHED ;

	========================================================================================================= */


%macro assign_ptnum(
	 indata=
	,othkeepvars=
	,ptvar=patientICN
	,ptvarlen=$10
	);

	%global ptcount;

	proc sort data=&indata OUT=all_CC (keep=&ptvar isCase &othkeepvars)
		NODUPKEY;
	by &ptvar; run;

	data
		all_cases
		all_controls
		;
	set all_CC;
	call streaminit(702385674);
	length r 8;
	r=rand('uniform');
	if isCase=1 then output all_cases;
	else if isCase=0 then output all_controls;
	run;

	data 
		pt2num (keep=fmtname type start label)
		num2pt (keep=fn2 t2 s2 l2 rename=(fn2=fmtname t2=type s2=start l2=label))
		all_controls (keep=&ptvar s2 isCase r &othkeepvars rename=(s2=ptnum))
		;
	set all_controls;
	length fmtname fn2 $8 type t2 $1 start &ptvarlen s2 6 label 6 l2 &ptvarlen;
	fmtname="$fpt2num"; fn2="fnum2pt";
	type='C'; t2='N';
	start=&ptvar; s2=_N_;
	label=_N_; l2=&ptvar;
	run;

	proc format cntlin=pt2num; run;
	proc format cntlin=num2pt; run;

	proc sql noprint; select count(&ptvar) into :ptcount from all_controls; quit;

%mend; *assign_ptnum();

%MACRO match(
	 sub_cases=
	,sub_controls=
	,matchvars=
	,lbl=
	,cntrls_per_case=
	,w_replace=0
	,ptvar=patientICN
	,match_snip=
	);

	%if &cntrls_per_case= %then %do;
		%let cntrls_per_case=1;
		%put ::: NOTE: using 1 control per case ;
	%end;
	%if &lbl= %then %do;
		%let lbl=---;
	%end;

	%let matchvars=%cmpres(&matchvars);

	%let mvlist=;
	%let mvplist=; * formats, ranges, macro snippets ;
	%let NMV=%sysfunc(countW(&matchvars,' '));
	%do mvi=1 %to &NMV;
		%let mv=%scan(%scan(&matchvars,&mvi,' '),1,:);
		%let mvlist=&mvlist &mv;
		%let mvp=%scan(%scan(&matchvars,&mvi,' '),2,:);
		%if &mvp^= %then %do;
			%if %sysfunc(anydigit(&mvp))^=1 and %substr(&mvp,1,1)^=< %then %do;
				%let mvp=%sysfunc(compress(&mvp,.));
			%end;
			%let mvplist=&mvplist &mvp;
		%end;
		%else %do;
			%let mvplist=&mvplist E;
		%end;
	%end;

	%let mvlist=%cmpres(&mvlist);
	%let mvplist=%cmpres(&mvplist);

	proc sql;
	create table matched as
	select a.&ptvar, b.ptnum as cntrl_pt, a.r * b.r as rand_sort 
	from
		&sub_cases A
		INNER join
		&sub_controls B
		on
		%do mvi=1 %to &NMV;
			%if &mvi>1 %then 
				AND;
			%let mv=%scan(&mvlist,&mvi,' ');
			%let mvp=%scan(&mvplist,&mvi,' ');
			%if &mvp=E %then %do;
				a.&mv=b.&mv
			%end;
			%else %do;
				%if %sysfunc(anydigit(&mvp))=1 OR %substr(&mvp,1,1)=< %then %do;
					%if %sysfunc(anydigit(&mvp))=1 %then %do;
						%let mvp=<=&mvp;
					%end;
					abs(a.&mv-b.&mv)&mvp
				%end;
				%else %if %index(&mvp,!) %then %do;
					%let mvp=%sysfunc(compress(&mvp,!));
					(%&mvp)
				%end;
				%else %do;
					put(a.&mv,&mvp..)=put(b.&mv,&mvp..)
				%end;
			%end;
		%end;
	order by a.&ptvar, rand_sort;
	quit;

	proc sql noprint;
	select put(nobs,comma20.) into :init_match_obs trimmed 
	from dictionary.tables 
	where libname='WORK' and upcase(memname)='MATCHED';
	quit;

	%put ::: (&lbl) The initial matched dataset contains &init_match_obs records;
	
	%if %length(&match_snip) %then %do;
		%&match_snip;
	%end;

	data _caselist_; set &sub_cases (keep=&ptvar); run;

	data 
		&sub_cases (keep=&ptvar caseID nc)
		&sub_controls (keep=cntrl_pt caseID)
		;
	set matched;
	by &ptvar;
	length caseID 5 nc 5;
	array T {&ptcount} $1 _temporary_;
	array RU {&cntrls_per_case} 5 _temporary_; * re-use pile ;
	if first.&ptvar then do;
		caseID+1;
		nc=0;
		nru=0;
		call missing(of RU[*]);
	end;
	if nc<&cntrls_per_case then do;
		if T[cntrl_pt]=' ' then do;
			T[cntrl_pt]='1';
			nc+1;
			output &sub_controls;
		end;
		else if nru<&cntrls_per_case then do;
			nru+1;
			RU[nru]=cntrl_pt;
		end;
	end;
	if last.&ptvar then do;
		%if &w_replace %then %do;
			i=0;
			do while (nc<&cntrls_per_case and i<nru);
				i+1;
				cntrl_pt=RU[i];
				nc+1;
				output &sub_controls;
			end;
		%end;
		if nc then output &sub_cases;
	end;
	run;

	proc datasets lib=work memtype=data nolist nodetails; delete matched; run; quit;

	proc sql noprint;
	select count(*) into :n_none from (select &ptvar from _caselist_ EXCEPT select &ptvar from &sub_cases);

	create table nc as select nc, count(*) as ncases from &sub_cases group by nc order by nc;
	
	select count(distinct cntrl_pt) into :n_unq_controls from &sub_controls;
	
	select count(cntrl_pt) into :n_tot_controls from &sub_controls;
	quit;

	data nc (keep=cntrl_cnt_:);
	set nc end=last;
	length cntrl_cnt_1-cntrl_cnt_&cntrls_per_case 5;
	array C {*} cntrl_cnt_:;
	retain C 0;
	C[nc]=ncases;
	if last then output;
	run;

	data nc;
	length lbl $32 w_replace 3 NO_CONTROLS 5;
	set nc;
	lbl="&lbl";
	w_replace=&w_replace;
	NO_CONTROLS=&n_none;
	length n_unq_controls n_tot_controls 5 matchvars $50;
	n_unq_controls=&n_unq_controls;
	n_tot_controls=&n_tot_controls;
	matchvars="&matchvars";
	run;

	proc append data=nc base=all_match_qc; run;
	
	data matched;
	set
		&sub_cases
		&sub_controls (in=B)
		;
	if B then &ptvar=put(cntrl_pt,fnum2pt.);
	keep &ptvar caseID nc;
	run;

	%if %symexist(msg_done)=0 %then %do;
		%global msg_done;
		%put ::: created QC dataset WORK.ALL_MATCH_QC;
	%end;
	%put ::: created WORK.matched  ...  LABEL: &lbl;

	proc sort data=matched; by &ptvar; run;

	proc datasets lib=work memtype=data nolist nodetails; delete &sub_controls &sub_cases _caselist_ nc; run; quit;

%MEND; *match();


