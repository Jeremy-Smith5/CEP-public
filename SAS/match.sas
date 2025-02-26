
/*

	Perform exact or range-based matching on two provided datasets <SUB_CASES> and <SUB_CONTROLS> on
	the provided variable list <MATCHVARS>.  See example call below, which is abstracted
	from <2020 ORD Covid Study>/Programs/post_covid_cx/phase1.mk.matched.pop.sas

	J Smith
	Sep 2022

	UPDATE 06 Dec 2022: match variables can now be specified to match within a given 
		range +/- or by means of a user-provided macro snippet (in addition to the original
		options of exact matching or format-based matching.  

		For exact matching, just specify the variable name.
		For format-based matching, separate variable name from format with a : (no period needed)
		For range matching, separate variable from number with : (by default, this will be assumed 
			as <= the given number.  To specify less than, just use < before number).
		For macro-based matching, separate variable name from macro snippet name with :  -- do NOT
			include a % sign - instead, use a ! sign, e.g., !state_match to run a macro called 
			%state_match

	UPDATE 25 Feb 2025: fixed deduplication issue that was making it impossible for people to contribute
		more than one record to the matching - e.g., as controls on separate dates and/or as
		both a case and control(s).  Also added a KEYVARS parameter to the MATCH macro that should
		be used to specify any variables other than the PTVAR variable that serve to uniquely 
		identify a record on the input dataset.  For example, if people can appear on separate
		dates but not more than once for a given date, then list the date variable in KEYVARS. If
		people can only appear once in the input dataset, then KEYVARS should be left blank. PTVAR
		will be assumed as one of the KEYVARS.  

	NOTES: 
		- currently, this macro does not allow people to contribute more than one CASE (isCase=1) 
		record.

		- this macro will exit automatically if a person has more than one CASE record or if 
		the KEYVARS do not uniquely identify records on the input dataset.

		- however, there is NO assumption that a person can only serve as a CONTROL (isCase=0) 
		BEFORE serving as a case - the macro is not verifying that the input data comply with 
		this since no date variable is required.  If this is a requirement, then then the user
		must assure this assumption is met prior to running the macro.  

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
				

	EXAMPLE CALL:
	
	Note that the macro 'minGap' is an optional user-provided snippet which is then called by
	the %match macro further down - this step can be anything required as long as it outputs
	a dataset called WORK.matched.


	%include "&CWD/match.sas";  * contains assign_ptnum() and match() ;

	%macro minGap;

		* snippet being run by match macro to minimize gap in lab dates ;
		proc sql undo_policy=NONE;
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
		,othkeepvars=specdate age male antvir &emvlist conf_:
		,ptvar=patientICN
		,ptvarlen=$10
		,check_data_only=0
		);

	%match(
		 sub_cases=scases
		,sub_controls=scontrols
		,matchvars=age:fage male antvir vaxdate:180 HHS:!hhsjoin &extra_match_vars
		,keyvars=SPECDATE
		,lbl=&oc2
		,cntrls_per_case=4
		,w_replace=1
		,ptvar=patientICN
		,match_snip=minGap
		);

		* outputs a dataset called WORK.MATCHED ;

	========================================================================================================= */


%macro assign_ptnum(  /* RUN THIS MACRO BEFORE RUNNING THE MATCH MACRO */
	 indata=	/* input data containing (<PTVAR>, isCase(0/1), <OTHKEEPVARS>) -  will not be overwritten. See rules in header. */
	,othkeepvars=	/* any variables required for matching or that uniquely identify records aside from PTVAR */
	,ptvar=patientICN  /* patient variable */
	,ptvarlen=$10	/* type/length of PTVAR */
	,check_data_only=0  /* 1: just perform basic QC of input data, then exit.  0: perform QC and continue */
	);  /* OUTPUTS: WORK.all_cases, WORK.all_controls and global macro variable PTCOUNTS */

	%global ptcount;

	* create a temp dataset ;
	proc sort data=&indata 
		OUT=all_CC (keep=&ptvar isCase &othkeepvars);
	by &ptvar; run;

	%let multi_case=0;

	* perform QC ;
	proc sql;
	title '# of people who appear as both cases and controls';
	select count(*) from 
		(select &ptvar, count(distinct isCase) as nvals from all_CC
		group by &ptvar HAVING nvals>1);

	title '# of people who appear as a control more than once';
	select count(*) from
		(select &ptvar, count(*) as nvals from all_CC
		WHERE isCase=0 group by &ptvar HAVING nvals>1);
	
	title '# of people who appear as a CASE more than once -- this is not allowed!';
	select count(*) into :multi_case trimmed from
		(select &ptvar, count(*) as nvals from all_CC
		WHERE isCase=1 group by &ptvar HAVING nvals>1);
	quit;

	%if &multi_case %then %do;
		%put ::: ERROR - your input dataset &indata contains people with >1 record where isCase=1 ;
		%abort cancel;
	%end;
	%if &check_data_only %then %do;
		%endsas;
	%end;
	
	* assign a random floating point number and split into cases / controls ;
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

	proc sort data=all_controls; by &ptvar; run;

	* for controls, create a number that uniquely identifies each PERSON and create forward and backward lookups ;
	data 
		pt2num (keep=fmtname type start label)
		num2pt (keep=fn2 t2 s2 l2 rename=(fn2=fmtname t2=type s2=start l2=label))
		all_controls (keep=&ptvar s2 isCase r &othkeepvars rename=(s2=ptnum))
		;
	set all_controls;
	BY &ptvar;
	length fmtname fn2 $8 type t2 $1 start &ptvarlen s2 6 label 6 l2 &ptvarlen;
	retain fmtname '$fpt2num' fn2 'fnum2pt' type 'C' t2 'N' s2 0 label 0;
	if first.&ptvar then do;
		s2+1;
		label+1;
		start=&ptvar;
		l2=&ptvar;
		output pt2num;
		output num2pt;
	end;
	output all_controls;  /* can be >1 row per person */
	run;

	proc format cntlin=pt2num; run;
	proc format cntlin=num2pt; run;

	proc sql undo_policy=NONE noprint; select count(start) into :ptcount trimmed from pt2num /*all_controls*/; quit;

%mend; *assign_ptnum();

%MACRO match(  /* RUN AFTER ASSIGN_PTNUM.  INPUT DATA SHOULD BE IN THE FORMAT OF work.all_cases AND work.all_controls created above */
	 sub_cases=	/* CASE data, e.g., WORK.all_cases */
	,sub_controls=  /* CONTROL data, e.g., WORK.all_controls */
	,matchvars=	/* variables for matching (see rules in header) - these must also have been included in the OTHERVARS arg of assign_ptnum */
	,keyvars=	/* see note in header - variable(s) aside from PTVAR that together form a unique key for the original input data */
	,lbl=		/* optional text string that will be used to label output - this is useful if running this on multiple populations in a loop */
	,cntrls_per_case=	/* maximum # of controls to identify per case - if blank, defaults to 1 */
	,w_replace=0	/* 0: perform matching without replacement, 1: perform matching with replacement */
	,ptvar=patientICN	/* patient variable */
	,match_snip=	/* see note in header - name of optional, user-defined macro for modifying the MATCHED dataset prior to final selection of controls */
	);	/* OUTPUTS: WORK.MATCHED, WORK.ALL_MATCH_QC */

	* process the input parameters ;
	%if &cntrls_per_case= %then %do;
		%let cntrls_per_case=1;
		%put ::: NOTE: using 1 control per case ;
	%end;
	%if &lbl= %then %do;
		%let lbl=---;
	%end;

	%let matchvars=%cmpres(&matchvars);

	%let nkv=0;
	%let keyvars_csv=;
	%if %length(&keyvars) %then %do;
		data _null_;
		kv=compbl(upcase("&keyvars"));
		kv=trim(tranwrd(kv,upcase("&ptvar"),""));
		call symputx("keyvars",kv);
		call symputx("nkv",countc(kv,' ')+1);
		call symputx("keyvars_csv",tranwrd(kv,' ',','));
		run;

		%if &nkv %then %do;
			data case_keys;
			set &sub_cases (keep=&ptvar &keyvars);
			run;
		%end;
	%end;

	* assure that KEYVARS + PTVAR unqiuely identify records ;
	proc sql noprint;
	select count(*) into :nrecs_control trimmed from &sub_controls;
	select count(*) into :nrecs_control_key from (select distinct &ptvar %if &nkv %then , &keyvars_csv; from &sub_controls);
	quit;

	%if &nrecs_control^=&nrecs_control_key %then %do;
		%put ::: STOPPING! the number of records in the control dataset is greater than the number of unique keys ;
		%put ::: STOPPING! specify variables other than &ptvar used to define a unique record in the KEYVARS parameter ;
		%put ::: ERROR: these variables should also be added to OTHKEEPVARS parameter in the call to assign_ptnum ;
		%abort cancel;
	%end;

	* process the matching variables ;
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

	* perform the initial join of cases to controls (many:many) ;
	proc sql undo_policy=NONE;
	create table matched as
	select a.&ptvar, b.ptnum as cntrl_pt, monotonic() as _recnum,
	%if &nkv %then %do; 
		%do kvi=1 %to &nkv; 
			%let kv=%scan(&keyvars,&kvi,' ');
			b.&kv,
		%end; 
	%end;
	a.r * b.r as rand_sort 
	from
		&sub_cases A
		INNER join  /* M:M */
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

	proc sql undo_policy=NONE noprint;
	select put(nobs,comma20.) into :init_match_obs trimmed 
	from dictionary.tables 
	where libname='WORK' and upcase(memname)='MATCHED';
	quit;

	%put ::: (&lbl) The initial matched dataset contains &init_match_obs records;
	
	* if provided, perform additional processing on intial matched dataset via a user-defined macro ;
	%if %length(&match_snip) %then %do;
		%&match_snip;
	%end;

	data _caselist_; set &sub_cases (keep=&ptvar); run;

	%let any_reuse=0;

	* make final selection of control(s) for each case, with or without replacement, as specified ;
	data 
		&sub_cases (keep=&ptvar caseID nc)
		&sub_controls (keep=cntrl_pt caseID &keyvars %if &w_replace %then matchrec;)
		;
	set matched;
	by &ptvar;
	length caseID 5 nc 5 matchrec 5;
	array T {&ptcount} $1 _temporary_;
	array CURR {&cntrls_per_case} 5 _temporary_;
	array RU {&cntrls_per_case} 5 _temporary_; * re-use pile ;
	array RUrec {&cntrls_per_case} 5 _temporary_;
	if first.&ptvar then do;
		caseID+1;
		nc=0;
		nru=0;
		matchrec=.;
		call missing(of CURR[*], of RU[*], of RUrec[*]);
	end;
	if nc<&cntrls_per_case then do;
		if T[cntrl_pt]=' ' then do;
			T[cntrl_pt]='1';
			nc+1;
			CURR[nc]=cntrl_pt;
			output &sub_controls;
		end;
		%if &w_replace %then %do;
			else if nru<&cntrls_per_case and cntrl_pt NOT in CURR and cntrl_pt NOT in RU then do;
				nru+1;
				RU[nru]=cntrl_pt;
				RUrec[nru]=_recnum;
			end;
		%end;
	end;
	if last.&ptvar then do;
		%if &w_replace %then %do;
			i=0;
			do while (nc<&cntrls_per_case and i<nru);
				call symputx("any_reuse",1);
				i+1;
				cntrl_pt=RU[i];
				matchrec=RUrec[i];
				nc+1;
				output &sub_controls;
			end;
		%end;
		if nc then output &sub_cases;
	end;
	run;

	%if &any_reuse %then %do;
		* for controls that were already used by other cases, go back and get the KEYVARS, if any, from the MATCHED dataset ;
		proc sql undo_policy=none;
		create table ru_subs as
		select a.*
		%if &nkv %then %do; 
			%do kvi=1 %to &nkv; 
				%let kv=%scan(&keyvars,&kvi,' ');
				,b.&kv
			%end;
		%end;
		from
			(select * from &sub_controls %if &nkv %then (drop=&keyvars); WHERE matchrec>.) A
			inner join
			matched B
			on a.matchrec=b._recnum;
		quit;

		data &sub_controls;
		set
			&sub_controls (where=(matchrec=.))
			ru_subs
			;
		run;
	%end;

	* clean up and make a QC dataset (one row with summary stats) ;
	proc datasets lib=work memtype=data nolist nodetails; delete matched %if &any_reuse %then ru_subs;; run; quit;

	proc sql undo_policy=NONE noprint;
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

	* final QC dataset - append to base in case running this macro in a loop on multiple populations ;
	proc append data=nc base=all_match_qc; run;

	* if KEYVARS were provided, add them back to the CASEs ;
	%if &nkv %then %do;
		proc sql undo_policy=none;
		create table &sub_cases as
		select a.*
		%do kvi=1 %to &nkv; 
			%let kv=%scan(&keyvars,&kvi,' ');
			,b.&kv
		%end;
		from
			&sub_cases A
			inner join  /* 1:1 */
			case_keys B
			on a.&ptvar=b.&ptvar;

		drop table case_keys;
		quit;
	%end;
	
	* stack cases with controls - the variable CASEID can be used to tie these together into matched sets ;
	data matched;
	length lbl $32;
	set
		&sub_cases
		&sub_controls (in=B)
		;
	lbl="&lbl";
	length isCase 3;
	isCase=1;
	if B then do;
		isCase=0;
		&ptvar=put(cntrl_pt,fnum2pt.);
	end;
	keep lbl &ptvar &keyvars caseID nc isCase;
	run;

	%if %symexist(msg_done)=0 %then %do;
		%global msg_done;
		%put ::: created QC dataset WORK.ALL_MATCH_QC;
	%end;
	%put ::: created WORK.matched  ...  LABEL: &lbl;

	* final matched dataset ;
	proc sort data=matched; by &ptvar; run;

	proc datasets lib=work memtype=data nolist nodetails; delete &sub_controls &sub_cases _caselist_ nc; run; quit;

%MEND; *match();


