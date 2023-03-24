/*

	Calculate cohort creation numbers for stepwise inclusions/exclusions
	based on a set of flag variables and/or incl/excl logic
	and create a simple diagram of the steps.  Output is
	a SAS dataset called 'FLOW' which is a text-based diagram 
	and which can, for ex., be exported to CSV and formatted as 
	needed.

	To use, specify the analytic dataset (incl. libname if not 
	WORK) containing all records (prior to any exclusions) and 
	also specify the variables to be used for inclusions/exclusions.  
	Steps will be carried out in the order that variables
	are specified.  NOTE that the dataset itself will not be 
	modified by this macro.  

	By default, variables are assumed to be typical 0/1 flags WHERE A
	1 INDICATES INCLUSION IN THE COHORT - 
	if this is the case, then simply list the variables (you can 
	use the usual SAS colon operator to indicate a list, e.g., flag_:, 
	BUT DO NOT mix that syntax with other variables.  For ex., if
	you wish to use the variables FEMALE FLAG1 FLAG2 FLAG3, you 
	must list them explicitly rather than using FEMALE FLAG:.  

	If all variables are 0/1, or, in fact, can be considered as 
	'false', which in SAS is missing (char or num) or zero,	or
	'true' (anything else) for the purposes of inclusion, then 
	the parameter FLAGVALS can be left blank.  If more complex 
	logic is needed (or if the 'true' value is to be used for EXclusion), 
	then FLAGVALS should be a pipe-separated
	list of logical statements that are in the same order as
	the corresponding variables listed in FLAGLIST.  See 
	examples below.  

	
	** EXAMPLE 1 **

	data simple;
	length pt flag1-flag5 3;
	label 
		flag1='cancer free in 6-month lookback'
		flag2='1+ dx for CHF'
		flag3='enrolled in FFS Pts A/B'
		flag4='had 1+ OP visit during lookback'
		flag5='current user of diuretics'
		;
	array f {*} flag:;
	array v {5} _temporary_ (0.85, .25, .7, .3, .6);
	do pt=1 to 1000;
		do i=1 to dim(f);
			f[i]=(ranuni(0)<v[i]);
		end;
		output;
	end;
	drop i;
	run;

	%flow(srcdat=simple, flaglist=flag:);

	OUTPUT:
		---------------------------------------------------------------------------------------
		START POP: 1000                                                           
		  |                                                                       
		  |                                                                       
		  V                                                                       
		CONDITION: flag1                                    ----->    REMOVED: 150
		  |                                                                       
		  V                                                                       
		[MET REQ. FOR: cancer free in 6-month lookback ]                          
		POP: 850                                                                  
		  |                                                                       
		  |                                                                       
		  V                                                                       
		CONDITION: flag2                                    ----->    REMOVED: 650
		  |                                                                       
		  V                                                                       
		[MET REQ. FOR: 1+ dx for CHF ]                                            
		POP: 200                                                                  
		  |                                                                       
		  |                                                                       
		  V                                                                       
		CONDITION: flag3                                    ----->    REMOVED: 58 
		  |                                                                       
		  V                                                                       
		[MET REQ. FOR: enrolled in FFS Pts A/B ]                                  
		POP: 142                                                                  
		  |                                                                       
		  |                                                                       
		  V                                                                       
		CONDITION: flag4                                    ----->    REMOVED: 101
		  |                                                                       
		  V                                                                       
		[MET REQ. FOR: had 1+ OP visit during lookback ]                          
		POP: 41                                                                   
		  |                                                                       
		  |                                                                       
		  V                                                                       
		CONDITION: flag5                                    ----->    REMOVED: 16 
		  |                                                                       
		  V                                                                       
		[MET REQ. FOR: current user of diuretics ]                                
		FINAL POP: 25                                                             
		---------------------------------------------------------------------------------------

	** EXAMPLE 2 **

	data complex;
	length pt 3 sex $1 age 3 state $2;
	label
		sex='patient sex'
		age='pt age as of cohort entry'
		state='pt state of residence at cohort entry'
		;
	array s {5} $2 _temporary_ ('AK', 'HI', 'CA', 'WA', 'OR');
	do pt=1 to 1000;
		sex='F';
		if ranuni(0)<0.45 then sex='M';
		age=int(ranuni(0)*50)+30;
		state=s[int(ranuni(0)*dim(s)+1)];
		output;
	end;
	run;

	%flow(srcdat=complex, flaglist=age sex state, flagvals=45<=age<65|sex='M'|state in ('HI','CA'));

	OUTPUT:	
		---------------------------------------------------------------------------------------
		START POP: 1000                                                                 
		  |                                                                             
		  |                                                                             
		  V                                                                             
		CONDITION: 45<=age<65                                     ----->    REMOVED: 620
		  |                                                                             
		  V                                                                             
		[MET REQ. FOR: pt age as of cohort entry ]                                      
		POP: 380                                                                        
		  |                                                                             
		  |                                                                             
		  V                                                                             
		CONDITION: sex='M'                                        ----->    REMOVED: 213
		  |                                                                             
		  V                                                                             
		[MET REQ. FOR: patient sex ]                                                    
		POP: 167                                                                        
		  |                                                                             
		  |                                                                             
		  V                                                                             
		CONDITION: state in ('HI','CA')                           ----->    REMOVED: 97 
		  |                                                                             
		  V                                                                             
		[MET REQ. FOR: pt state of residence at cohort entry ]                          
		FINAL POP: 70                                                                   

		---------------------------------------------------------------------------------------

	J Smith
	20 Aug 2019
	
	Modification history: NN 01 Apr 2021!
	1) replaced the hard-coded length for step variable with macro variable label_len: the pre-set value for this macro is 120 char!!	
	================================================================= */

%MACRO flow(
	srcdat=,
	flaglist=,
	flagvals=,
	squash=1,
	order_by_alpha=1,	/* if 1, order flags in <FLAGLIST> alphabetically ... if 0, use dataset var order (if no 
				colon was used in <FLAGLIST>, just use order as stated in list) */
	allowFlagX=1,
	label_len=120,
	out_rscript=,
		rscript_incl_vname=1);

	%let srcdat=%upcase(&srcdat);
	%let srclib=WORK;
	%if %index(&srcdat,.) %then %do;
		%let srclib=%scan(&srcdat,1,.);
		%let srcdat=%scan(&srcdat,2,.);
	%end;
	
	%if %index(&flaglist,:) %then %do;
		%let rawflags=&flaglist;
		%let flaglist=;
		%let rawflags=%cmpres(&rawflags);
		%let nrawflags=%sysfunc(countW(&rawflags,' '));
		%do fi=1 %to &nrawflags;
			%let rfi=%scan(&rawflags,&fi,' ');
			%if %index(&rfi,:) %then %do;
				proc sql noprint;
				select name into :subflaglist separated by ' '
				from dictionary.columns where libname="&srclib"
				and upcase(memname)="&srcdat" and 
				upcase(substr(name,1,%length(&rfi)-1)) = 
					upcase("%substr(&rfi,1,%length(&rfi)-1)")
				%if &order_by_alpha %then order by name;;
				quit;
				%let flaglist=&flaglist &subflaglist;
			%end;
			%else %do;
				%let flaglist=&flaglist &rfi;
			%end;
		%end;
	%end;
	%let nf=%sysfunc(countW(&flaglist,' '));
	%do i=1 %to &nf;
		%let f&i=%scan(&flaglist,&i,' ');
		%let v&i=%scan(&flaglist,&i,' ');
		%if %length(&flagvals) %then %do;
			%let v&i=%scan(&flagvals,&i,|);
		%end;
	%end;
	proc sql;
	%do i=1 %to &nf;
		create table _t&i as 
		select name, upcase(char(type,1)) as type
		from dictionary.columns where libname="&srclib"
		and upcase(memname)="&srcdat" and 
		upcase(name) = upcase(strip("%scan(&flaglist,&i,' ')"));
	%end;
	quit;

	data _alltypes;
	set 
		%do i=1 %to &nf; 
			_t&i 
		%end;
		;
	run;

	proc datasets lib=work memtype=data nolist nodetails;
	delete _t:;
	run; quit;

	%let anychar=0;
	%let anynum=0;
	%let isnum=;
	%let cvars=;
	%let nvars=;

	data _null_;
	set _alltypes END=LAST;
	array numeric {&nf}  _temporary_;
	array cvars {&nf} $32 _temporary_ (&nf*'');
	array nvars {&nf} $32 _temporary_ (&nf*'');
	retain i 0 ci 0 ni 0;
	i+1;
	numeric[i]=(type='N');
	if type='C' then do;
		ci+1;
		call symputx('anychar',1);
		cvars[ci]=upcase(compress(name));
	end;
	if type='N' then do;
		ni+1;
		call symputx('anynum',1);
		nvars[ni]=upcase(compress(name));
	end;
	if LAST then do;
		call symput("isnum",catx(',', of numeric[*]));
		if ci then call symput("cvars",catx(' ', of cvars[*]));
		if ni then call symput("nvars",catx(' ', of nvars[*]));
		call symputx("ncv",ci);
		call symputx("nnv",ni);
	end;
	run;

	%put :: ISNUM: &isnum;
	%put :: CVARS: &cvars;
	%put :: NVARS: &nvars;	

	data 
		flow (keep=remaining arrows minus)
		squashed (keep=step before removed)
		;
	set &srclib..&srcdat (keep=&flaglist) END=LAST;
	array _F {&nf} _temporary_; * flag counts ;
	array _FL {&nf} $&label_len _temporary_; * flag labels ; /* NN: original hard-coded length was 75 */
	array _FLOG {&nf} $100 _temporary_; * flag logic ;
	array isnum {&nf} _temporary_ (&isnum); * numeric var indicators ;
	length junkchar $1 junknum 3;
	junkchar='';
	junknum=.;
	%if &anychar %then %do;
		array _cv {*} &cvars; * character flag variables ;
	%end;
	%else %do;
		array _cv {*} junkchar;
	%end;
	%if &anynum %then %do;
		array _nv {*} &nvars; * numeric flag variables ;
	%end;
	%else %do;
		array _nv {*} junknum;
	%end;
	_cont=1;
	_ncv=1;
	_nnv=1;
	%do i=1 %to &nf;
		if _cont then do;
			if 
			%if &flagvals= %then %do;
				%if &allowflagX %then %do;
					%if %sysfunc(prxmatch(%nrbquote(m/flag\d+x/i),&&v&i)) %then %do;
						(&&v&i=0)
					%end;
					%else %do;
						&&v&i
					%end;
				%end;
				%else %do;
					&&v&i
				%end;
			%end;
			%else %do;
				&&v&i
			%end;
			then _F[&i]+1;
			else _cont=0;
		end;
		if isnum[&i] then do;
			_FL[&i]=coalesceC(vlabel(_nv[_nnv]), vname(_nv[_nnv]));
			_nnv+1;
		end;
		else do;
			_FL[&i]=coalesceC(vlabel(_cv[_ncv]), vname(_cv[_ncv]));
			_ncv+1;
		end;
		_FLOG[&i]="&&v&i";
	%end;
	if LAST then do;
		totobs=_N_;
		length remaining step $&label_len arrows $6 minus $120 kw $11 before removed 8; /*NN: replced hard-coded length for step with the macro var */
		remaining=compbl("START POP: " || put(_N_,comma20.)); output flow;
		/*
		step="START POP"; before=_N_; removed=.; output squashed; */
		totremoved=0;
		do i=1 to dim(_F);
			remaining="  |  "; output flow;
			remaining="  |  "; output flow;
			remaining="  V  "; output flow;
			remaining=compbl("CONDITION: " || _FLOG[i]); 
			arrows="----->"; 
			minus=compbl("REMOVED: " || put(totobs-totremoved-_F[i],comma20.)); output flow;
			remaining="  |  "; arrows=""; minus=""; output flow;
			remaining="  V  "; output flow;
			kw="POP: ";
			if i=dim(_F) then kw="FINAL POP: ";
			if _FL[i]^=_FLOG[i] then do;
				remaining=compbl("[MET REQ. FOR: " || _FL[i] || "]"); output flow;
				step=compbl("(" || _FLOG[i] || "): " || _FL[i]);
			end;
			else step=_FLOG[i];
			step=tranwrd(step,' )',')');
			before=(totobs-totremoved);
			removed=(before-_F[i]);
			output squashed;
			totremoved+(totobs-totremoved-_F[i]);
			remaining=compbl(kw || put(totobs-totremoved,comma20.)); output flow;
		end;
		step='FINAL POP';
		before=(totobs-totremoved);
		removed=.;
		output squashed;
	end;
	run;

	title "COHORT FLOW";
	proc print data=%if &squash %then squashed; %else flow; noobs width=min;
	%if &squash %then format before removed comma20.;; 
	run;

	%if %length(&out_rscript) %then %do;
	
		%if %length(&rscript_incl_vname)=0 %then %let rscript_incl_vname=1;;

		data for_r;
		set squashed end=last; 
		length lastlab $300 mainbox $400;
		if &rscript_incl_vname=0 then do;
			step=substr(step,index(step,':')+2);
		end;
		lastlab=lag(step);
		if _N_=1 then lastlab='Starting population';
		mainbox=compbl('add_box(txt = "' || '(n =' || put(before,comma20.) || '): '|| lastlab || '", text_width=50)'); 
		if not last then do;
			mainbox=compbl(mainbox || ' %>% '); 
		end;
		output;
		if not last then do;
			mainbox=compbl('add_side_box(txt = "Excluded (n =' || put(removed,comma20.) || ')")' || ' %>% '); 
			output;
		end;
		label mainbox="# Attrition diagram R script for &srcdat";
		keep mainbox;
		run;

		data header;
		length mainbox $400;
		mainbox='# run this in R Studio or similar'; output;
		mainbox="# source program: &sasprog"; output;
		mainbox="# date: &sysdate9"; output;
		mainbox="# NOTES: use cex option below to adjust text size, use text_width to control wrapping"; output;
		mainbox=' '; output;
		mainbox='library(consort)'; output;
		mainbox='library(magrittr)'; output;
		mainbox='library(grid)'; output;
		mainbox='options(txt_gp = gpar(cex = 0.8))'; output;
		mainbox=' '; output;
		mainbox='g <- '; output;
		run;

		data footer;
		length mainbox $400;
		mainbox=' '; output;
		mainbox='plot(g)'; output;
		mainbox=compbl('png(' || compress('"' || "P:\\&PROJ\\&srcdat._attrition.png" || '"') || 
			", height = 20, width = 9, units = 'cm', bg = 'transparent', res = 100, type = 'cairo')"); output;
		mainbox='plot(g)'; output;
		mainbox='dev.off()'; output;
		run;

		data for_r;
		set
			header
			for_r
			footer
			;
		run;

		proc printto new file="&CWD/&out_rscript"; run;

		ods noptitle;
		title;
		proc print data=for_r noobs width=min label; run;

		proc printto; run;

		%put ::: R script for attrition diagram was written to &CWD/&out_rscript;
	
	%end; 

%MEND; *flow();	

