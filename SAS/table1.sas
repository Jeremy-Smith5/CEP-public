
/* 
	Create a typical "table 1"-style demographics table from an input file that is one row
	per person and contains stratification and row variables requested in the macro call.
	
	Both the STRATVARS and ROWVARS arguments are pipe (|) - separated lists of variables.  A 
	user-defined format may be applied to any of these variables using the <var>:<format> 
	syntax (variable name and format separated by a colon).  No dot is necessary after the
	format name, but format names should begin with '$' if specifying a character format.  If
	no user-defined format is applied, the macro will first check whether a format is applied
	to that variable in the input dataset, in which case it will use that format.  If no format
	is applied and the variable is a stratifying variable, the macro will make a temporary
	format based on levels it finds for that variable in the data and apply that format - this
	will not change the underlying values.  If the variable is a row variable and no statistics 
	(e.g., mean stdev) are requested, the macro will first check whether the variable is a 
	0/1 binary variable with no missing values.  If it meets those conditions, the macro will
	only show the count and % for the <TRUE> (i.e., 1 or '1') level as is common in 
	demographics tables.  If it does not meet these conditions, the macro will create a format
	as described above for stratifying variables.
	
	***********************************************************************************************
	IMPORTANT: whether you are applying a user defined format, making use of a format that 
	already exists on the input dataset, or allowing the macro to make its own format 
	based on individual values of the variable, all levels MUST BE VARIABLE-NAME FRIENDLY, that
	is, they must contain only letters, numbers and underscores (no spaces), must start with
	a letter and be <=32 characters in length (shorter if letting the macro create a format).  
	For stratifying variables, levels should be kept as short as possible if stratifying by more
	than one variable because the column names in the output table will be underscore-separated
	concatenations of each unique combination of stratifying levels, e.g., MALE_BLACK_AGE3034.
	The total length of these concatenated strings cannot exceed 32 characters!!
	
	>>>>>>>>>>>>>>>>>>>>>>>>>>>>>	KEEP THINGS SHORT!   <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

	Here are some examples of variable name-friendly levels:
		age5064
		native_amer
		_2029
		age50to64
		cat3

	Here are some examples of levels that are NOT variable name-friendly!:
		2029
		age50-64
		native amer
		(missing)
		3

	Note that the variable name-friendly requirement appplies only to the FORMATTED values (assuming
	you have supplied a format, either in the macro call or in the source dataset).  

	***********************************************************************************************
	
	The output table can be stratified by multiple variables - each unique combination of
	the levels of these variables will be displayed as columns in the output along with each
	unique 'overall' grouping.  If a format is applied, the output will contain *all* 
	levels of the format, regardless of whether the input dataset contains all levels - for 
	instance, if one of the stratifying variables is age group, and age group contains an 
	'85-99' level but no one in the dataset is in this age group, that column will still 
	appear in the output data.  The same principal applies to row variables.
	
	Row variables can be one of several types:  1) a variable with a format applied in the form 
	<var>:<format>, 2) a variable with a format applied to the dataset, 3) a variable that is a 0/1 
	binary variable	with no missing values, 4) a variable for which the user is (implicitly) allowing
	the macro to create its own format (for instance, you have a variable called REGION that has
	levels Southeast, Northeast, Central, West), or finally 5) a variable for which the user
	is requesting specific statistics available from PROC MEANS (mean median std range iqr 
	p1 p5 p25 p75 p95 p99 min max, etc.).  
	
	For the first 4, the output will always be in the usual count / percent format, with counts 
	in the first column for the relevant stratification	and percentages in the second.  Updates 
	to this macro will provide options to format these as character strings with commas, percent 
	signs, parentheses, etc.  
	
	For statistical requests (#5 above), the user should separate the variable 
	from (up to 2) statistics with a slash (/), e.g., AGE/mean std   At this time, only 
	2 statistics per variable can be requested.  If further stats are needed, you can request
	them on a separate row, e.g., AGE/median iqr.  Note that each statistic will occupy a 
	column and will be presented as a single number, even though some statistics (RANGE, IQR)
	are typically presented as a range.  Improvements to this macro will include various 
	formatting options for the output, including transforming such statistics into their 
	usual form.
	
	Further notes:
	The macro will automatically add the typical 'N' row to the top of the table.
	
	Missing values: 
		if a count variable contains missing values, a 'missing' category will 
		automatically be added in the output.  
		
		if a 'stats' variable (i.e., one for which you are requesting mean, median,
		etc.) has missing values, the stats will be calculated as usual (ignoring
		missing values) and a separate 'missing' row for that variable will be
		output immediately below the stats row showing counts of missing values
		by stratum.
	
	IMPORTANT: avoid using the word 'ALL' as a level for a stratifying variable -- this will 
	conflict with the macro's attempt to create overall categories.

	Leave missing values as missing - the macro will handle these.

	If you allow the macro to create its own format for a row or stratifying variable, be aware
	that it will create levels by concatenating the variable NAME with the variable VALUE, e.g.,
	if you have a variable RACE with levels 'black', 'white', 'asian'... , the resulting auto-
	created levels will be 'RACE_black', 'RACE_white', 'RACE_asian', etc.  Keep this in mind 
	when thinking about the resulting variable lengths!

	MAKE SURE, if you are using more than one stratifying variable, that there is no overlap
	between the variables in terms of level names!  This will cause errors.  For this reason,
	avoid generic level names for stratifying variables such as 'OTHER', 'NONE', etc.

	NOTE this macro just produces a temporary (WORK) dataset called 'table1', so don't forget
	to save the output after the macro call!

	UPDATE Feb 2021: added option to create p-values for certain comparisons:
		- Cochran Mantel Haenszel chi sq for categorical variables
		- T-tests for comparison of means (ONLY if one stratifying variable with two levels)
		- Kruskal Wallis for comparison of medians (ONLY if one stratifying variable with two levels)
	
	UPDATE May 2021: added option to create standardized mean difference values for comparison of means and row-wise proportions

	UPDATE 2022: added option for ROWVARS that allows use of an '@' sign as a wildcard (which works here
		the same way as the usual SAS ':' wildcard.  So instead of typing out a list of 
		variables with a common prefix and separating them with pipes, you can just type the
		prefix followed by @.  For ex:
			rowvars=age:fage | age/mean | priority | conf@ | ever_hosp,
			...instead of 
			rowvars=age:fage | age/mean | priority | conf_ca | conf_chf | conf_copd | conf_dm | ever_hosp

	NOTES on PrintSMD option:
		1. Quantitative variables must be specified with '/mean std' as the requested statistics. Othwerwise, SMD will not print.
		2. If you want a single SMD for a categorical variables with >2 levels, see the %stddiff macro (stddiff.sas). PrintSMD only supports row-wise SMDs.
		3. PrintSMD is only valid for two-level stratifying variables

	Sample call:
		
		-----------------------------------------------	
		%table1(
			personfile=test,
			stratvars=
				urh | 
				sex:fsex
				,
			rowvars=
				race | 
				age:fage |
				age/mean std | 
				diabetes | 
				CHF |
				PAO2>:fHLN	 <- use '>' flag to ignore missing values in calc. of percentages 
				,
			pvalues=1
			);

		data coh.table_2015 (label=&sasprog);
		set table1;
		run;
		-----------------------------------------------	

	
	Jeremy Smith
	Feb 2018
	
	Last update: Dec 2022
	
	======================================================================================== */

%macro isCharacter(chkchar);
	%if %symexist(isCharacter) %then %do;
		%symdel isCharacter;
	%end;
	%global isCharacter;
	proc sql noprint;
	select count(*) into :vexists from pfile_vars where name=upcase("&chkchar");
	%if &vexists %then %do;
		select (type='char') into :isCharacter from pfile_vars where name=upcase("&chkchar");
		quit;
	%end;
	%else %do;
		quit;
		%put :: ERROR: the variable &V does not exist in your input file! -- SAS is quitting! ;
		%abort;
	%end;
%mend;

%MACRO table1(
	personfile=,	/* name of person-level input file containing STRATVARS and ROWVARS */
	stratvars=,		/* pipe-separated list of stratifying variables as described above */
	rowvars=,		/* pipe-separated list of row variables as described above */
	uselabels=0,	/* if set to 1 and variable label on input dataset (<personfile>) is <=32 characters, use var label instead of name */
	usecase=0,	/* if set to 1, use case of variable name as provided in macro call (unless using label) */
	pvalues=0,	/* 1: will perform an M-H chi-square test for categorical variables, a T-test for 2-way means, KW for 2-way medians */
	printSMD=0
	);

	options minoperator;
	
	data pfile_temp;
	set &personfile;
	run;
	
	proc sql;
	create table pfile_vars (rename=(vnm=name)) as 
	select upcase(name) as vnm, type, format, length, label
	from dictionary.columns where libname='WORK' and lowcase(memname)='pfile_temp'
	order by vnm;
	quit;
	
	%let nstrat=%eval(%sysfunc(countc(&stratvars,|))+1);

	%if &printSMD^=1 or &nstrat>2 %then %let printSMD=0;;

	%let svarlist=;
	%let qsvarlist=;
	%do i=1 %to &nstrat;
		%let sraw=%upcase(%sysfunc(compress(%scan(&stratvars,&i,|))));
		data _null_;
		call symput("sraw",compress("&sraw",compress("&sraw",'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_:')));
		run;
		%if %index(&sraw,:) %then %do;
			%let S&i=%scan(&sraw,1,:);
			%let Sf&i=%scan(&sraw,2,:);
			*isVNfriend(&&Sf&i,F);
			%isCharacter(&&S&i);
			%if &ischaracter and %substr(&&SF&i,1,1)^=$ %then %do; %let Sf&i=$&&Sf&i; %end;
		%end;
		%else %do;
			%let S&i=&sraw;
			%let Sf&i=;
			%isCharacter(&&S&i);
			* check for format applied to data ;
			proc sql noprint;
			select case when type='char' then compress('$' || compress(format,'$.')) else compress(format,'.') end into :Sf&i 
			from pfile_vars where upcase(name)="&&S&i" and not missing(format);
			quit;
			data _null_;
			* this is for the purpose of ignoring formats like '$20', which cause problems and are not useful here ;
			call symputx("fmtOK",anyalpha(compress("&&Sf&i")));
			run;
			%if &fmtOK=0 %then %do;
				%let Sf&i=;
			%end;
			%if &&Sf&i= %then %do;
				* make a format ;
				proc sql;
				create table mkfmt as select distinct "fsfmt" as fmtname, %if &ischaracter %then 'C'; %else 'N'; as type,
				&&S&i as start, &&S&i as end, compress("&&S&i.._" || %if &ischaracter %then &&S&i; %else put(&&S&i,8.);) as label from pfile_temp order by start;
				quit;
				%let Sf&i=fsfmt;
				%if &ischaracter %then %let Sf&i=$fsfmt;

				proc format cntlin=mkfmt; run;
			%end;
		%end;
		%let Sf&i=%sysfunc(compress(&&Sf&i));
		/*%if &isCharacter=0 %then %do;*/
			data pfile_temp (rename=(&&S&i..2=&&S&i));
			set pfile_temp;
			&&S&i..2=put(&&S&i,&&Sf&i...);
			if missing(&&S&i..2) then &&S&i..2='z';
			drop &&S&i;
			run;
		/*%end;*/
		%let svarlist=&svarlist &&S&i;
		%let qsvarlist=&qsvarlist %sysfunc(compress("&&s&i"));	
	%end; *nstrat loop;

	%let ttestOK=0;
	%if &nstrat=1 %then %do;
		proc sql noprint;
		select (count(distinct &S1)=2) into :ttestOK from pfile_temp;
		quit;
	%end;
	%if &printSMD=1 %then %do;
		%if &ttestOK=0 %then %do;
			%let printSMD=0;
			%put ::: sorry, printSMD only works when there is 1 stratifying variable and it has only 2 levels ;
		%end;
	%end;

	/*
	%if %substr(&rowvars,%length(&rowvars),1)=| %then %do;
		%let rowvars=%substr(&rowvars,1,%eval(%length(&rowvars)-1));
	%end;*/
	%let nvars=%eval(%sysfunc(countc(&rowvars,|))+1);

	data _null_;
	call symputx("hasWC",index("&rowvars","@")>0);
	run;

	%if &hasWC %then %do;
		%let _rvars=&rowvars;
		%let rowvars=;
		%do i=1 %to &nvars;
			%let _rv=%upcase(%scan(&_rvars,&i,|));
			%if &i>1 %then %do;
				%let rowvars=&rowvars|;
			%end;
			%if %substr(&_rv,%length(&_rv),1)=@ %then %do;
				%let _lrv=%length(&_rv);
				%let nwc=0;

				proc sql;
				create table wcvars as
				select name 
				from dictionary.columns where libname='WORK'
				and upcase(memname)='PFILE_TEMP' and upcase(substr(name,1,&_lrv-1))=substr("&_rv",1,&_lrv-1)
				order by name;

				select count(*) into :nwc from wcvars;
				quit;

				%if &nwc=0 %then %do;
					%put :: no variables matched the prefix &_rv - SAS is quitting! ;
					%abort cancel;
				%end;

				data wcvars;
				set wcvars end=last;
				array T {&nwc} $32 _temporary_;
				T[_N_]=name;
				if last then call symputx("wcvars",catx('|', of T[*]));
				run;
				
				/*
				proc sql noprint;
				select name into :wcvars separated by '|'
				from dictionary.columns where libname='WORK'
				and upcase(memname)='PFILE_TEMP' and upcase(substr(name,1,&_lrv-1))=substr("&_rv",1,&_lrv-1);
				quit; */
				
				%put ::: the table1 macro expanded the wildcard variable &_rv;
				%let rowvars=&rowvars &wcvars;
			%end;
			%else %do;
				%let rowvars=&rowvars &_rv;
			%end;
		%end;
		%let nvars=%eval(%sysfunc(countc(&rowvars,|))+1);
	%end;

	%let vtype_error=0;
	%let misslist=999;
	%let anymissingvals=0;
	%let IMvars=;
	%let hasheaderrow=0;
	
	%do i=1 %to &nvars;

		%let vraw=%upcase(%cmpres(%scan(&rowvars,&i,|)));
		%if %index(&vraw,BLANKROW) %then %do;
			%let header=;
			%let doubleblank=0;
			%if &hasheaderrow %then %let doubleblank=1;;
			%let hasheaderrow=1;
			%if %index(&vraw,:) %then %do;
				%let header=%scan(&vraw,2,:);
			%end;
			%goto nextvar;
		%end;
	
		%let ignoreMiss=0;
		%if %index(&vraw,%str(>)) %then %do;
			%let ignoreMiss=1;
			%let IMvars=&IMvars %scan(&vraw,1,%str(>));
		%end;
		%let isRedo=0;
		
		%redo:
		
		%let hasMiss=0;

		data _null_;
		call symput("vraw",compress("&vraw",compress("&vraw",'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_ :/')));
		run;
		%let vraw=%sysfunc(tranwrd(&vraw,%str(: ),%str(:)));
		%let vraw=%sysfunc(tranwrd(&vraw,%str( :),%str(:)));

		%let vraw=%sysfunc(tranwrd(&vraw,%str(/ ),%str(/)));
		%let vraw=%sysfunc(tranwrd(&vraw,%str( /),%str(/)));
		
		%if %index(&vraw,:) %then %do;
			%let vtype=F;
			%if %index(&vraw,%str( )) or %index(&vraw,%str(/)) %then %let vtype_error=1;
			%let V=%scan(&vraw,1,:);
			%let Vf=%scan(&vraw,2,:);
			%isCharacter(&V);
			%if &ischaracter and %substr(&Vf,1,1)^=$ %then %do; %let Vf=$&Vf; %end;
			proc format cntlout=outfmt; select &Vf; run;
			proc sql noprint;
			select (count(*)=0) into :nosuchfmt from outfmt;
			select distinct label into :dummies separated by " " from outfmt;
			quit;
			%if &nosuchfmt %then %do;
				%put :: ERROR: the format &Vf was not found! SAS is quitting.;
				%abort cancel;
			%end;
		%end;
		%else %if %index(&vraw,%str(/)) %then %do;
			%let vtype=S;
			%if %scan(&vraw,2,%str(/))= %then %let vtype_error=1;
			%let V=%scan(&vraw,1,%str(/));
			%let Vf=;
			%let Vs=%scan(&vraw,2,%str(/));
			%if &Vs= %then %let vtype_error=1;
			* confirm <V> is a numeric variable ;
			%isCharacter(&V);
			%if &isCharacter %then %let vtype_error=1;
			%let nVs=%sysfunc(min(2,%eval(%sysfunc(countc(&Vs,%str( )))+1)));  * for now, no more than 2 allowed ;
			%let doMean=0;
			%let doMedian=0;
			%do x=1 %to &nVs;  
				%let Vs&x=%scan(&Vs,&x,%str( ));
				%if %lowcase(&&Vs&x)=mean %then %do; %let doMean=1; %end;
				%if %lowcase(&&Vs&x)=median or %lowcase(&&Vs&x)=med %then %do; %let doMedian=1; %end;
			%end;
		%end;
		%else %if %index(&vraw,%str( )) %then %do;
			%let vtype_error=1;
		%end;
		%else %do;
			%let V=&vraw;
			%let Vf=;
			%put VVVV: &V;
			* check for a format applied to the input data for variable <V> ;
			proc sql noprint;
			select case when type='char' then compress('$' || compress(format,'$.')) else compress(format,'.') end into :Vf 
			from pfile_vars where upcase(name)="&V" and not missing(format);
			quit;
			data _null_;
			* this is for the purpose of ignoring formats like '$20', which cause problems and are not useful here ;
			call symputx("fmtOK",anyalpha(compress("&Vf")));
			run;
			%if &fmtOK=0 %then %do;
				%let Vf=;
			%end;
			%if &Vf^= %then %do;
				%let Vf=%sysfunc(compress(&Vf));
				proc format cntlout=outfmt; select &Vf; run;
				proc sql noprint;
				select (count(*)=0) into :nosuchfmt from outfmt;
				select distinct label into :dummies separated by " " from outfmt;
				quit;
				%if &nosuchfmt %then %do;
					%put :: ERROR: the format &Vf was not found! SAS is quitting.;
					%abort cancel;
				%end;
				%let vtype=F;
			%end;
			%else %do;
				* check whether binary (and no missing values);
				%isCharacter(&V);
				proc sql noprint;
				select (count(*)=0) into :isBin from pfile_temp where &V NOT in 
				%if &ischaracter %then ('0','1'); %else (0,1); /*and not missing(&V)*/;
				quit;
				%if &isBin %then %do;
					%if &ischaracter %then %do;
						%let Vf=$fdumb;
						proc format;
						value $fdumb
							'0'="not&V"
							'1'="&V"
							;
						run;
					%end;
					%else %do;
						%let Vf=fdumb;
						proc format;
						value fdumb
							0="not&V"
							1="&V"
							;
						run;
					%end;
					%let dummies=not&V &V;
					%let vtype=D; /* dummy */
				%end;
				%else %do;
					* make a format ;
					proc sql noprint;
					create table mkfmt as select distinct "ffmt" as fmtname, %if &ischaracter %then 'C'; %else 'N'; as type,
					&V as start, &V as end, compress("_f_" || %if &ischaracter %then &V; %else put(&V,8.);) as label 
					from pfile_temp where not missing(&V) order by start;
					select distinct label into :dummies separated by " " from mkfmt;
					quit;
					proc format cntlin=mkfmt; run;
					%put DUMMIES: &dummies;
					
					%if &ischaracter %then %let Vf=$ffmt;
					%else %let Vf=ffmt;
					%let vtype=F;
				%end;
			%end;
		%end;
	
		proc sql noprint;
		select label into :vrawlabel from pfile_vars
		where name="&V";
		quit;

		%let islabel=1;
		%let _vraw=%cmpres(%scan(&rowvars,&i,|));
		%let _vraw=%substr(&_vraw,1,%length(&V));
		%let ispropcase=0;
		%if &_vraw=%sysfunc(propcase(&_vraw)) %then %let ispropcase=1;;

		%if %length(&vrawlabel)>32 or %length(&vrawlabel)=0 or &uselabels=0 or &ispropcase %then %do;
			%let vrawlabel=&_vraw;
			%let islabel=0;
		%end;
		%if &usecase=0 and &islabel=0 %then %let vrawlabel=%upcase(&vrawlabel);;
	
		%if &vtype=F or &vtype=D %then %do;

			data pfile_slice;
			set pfile_temp;
			length _miss &dummies _missV 3;
			array _d {*} &dummies;
			_miss=0;
			_missV=0;
			if missing(&V) and "&Vf"^="FMISSVAL" then _miss=1;
			else do;
				if missing(&V) then do;
					_missV=1;
					&V=-999;
				end;
				do _i=1 to dim(_d);
					_d[_i]=(upcase(put(&V,&Vf..))=upcase(vname(_d[_i])));
				end;
				if _missV then &V=.;
			end;
			drop _i _missV;
			run;

			proc means data=pfile_slice completetypes chartype NOPRINT;
			class %do x=1 %to &nstrat; &&S&x %end; / preloadfmt;
			var %if &vtype=F %then _miss; &dummies;
			output out=means_slice
				%if &vtype=F %then sum(_miss)= ;
				%do x=1 %to %sysfunc(countW(&dummies));
					%let dvar=%scan(&dummies,&x,%str( ));
					sum(&dvar)= 
				%end;
					/ AUTONAME;
			run;

			%let pval_done=0;
			%if &pvalues %then %do;
				ods listing close;
				ods output CMH=pval;
					* NOTE: chi-square tests done here IGNORE missing values ;
					%if &isRedo %then %do;
						* CMH does not work unless >1 non-missing levels for each var, 
						regardless of formatting - temporarily change to -999 ;
						data pfile_temp;
						set pfile_temp;
						_missV=0;
						if missing(&V) then do;
							_missV=1;
							&V=-999;
						end;
						run;
					%end; 
					proc freq data=pfile_temp;
					%if &isRedo %then %do;
						format &V fMISSval.;
					%end;
					%else %do;
						format &V &Vf..;
					%end;
					table &S1 *
						%if &nstrat>1 %then %do;
							%do x=2 %to &nstrat; &&S&x * %end;
						%end; 
						&V / CMH;
					run;
					%if &isRedo %then %do;
						data pfile_temp;
						set pfile_temp;
						if _missV then &V=.;
						drop _missV;
						run;
					%end;
				ods output close;
				ods listing;
				
				%if %sysfunc(exist(pval)) %then %do;
					data pval;
					set pval;
					WHERE lowcase(althypothesis)='general association';
					length test_stat $4 test_stat_val 8 pval 8;
					test_stat="Chi2";
					test_stat_val=value;
					pval=prob;
					keep test_stat: pval;
					run;
					
					proc sort data=pval nodupkey; by test_stat; run;
					%let pval_done=1;
				%end;
			%end;
		%end;
		%else %do;
			%if &ischaracter %then %do;
				%put :: ERROR: you requested non-count statistics for the character variable &V ;
				%put :: SAS is quitting! ;
				%abort;
			%end;
			proc means data=pfile_temp completetypes chartype NOPRINT;
			class %do x=1 %to &nstrat; &&S&x %end; / preloadfmt;
			var &V;
			output out=means_slice
				nmiss(&V)=
				%do x=1 %to &nVs;
					&&Vs&x(&V)= 
				%end;
					/ AUTONAME;
			run;

			%let pval_done=0;
			%if &pvalues=1 and &ttestOK=1 %then %do;
				ods listing close;
				%if &doMean %then %do;
					ods output TTests=pval;
						proc ttest data=pfile_temp;
						class &S1;
						var &V;
						run;
					ods output close;
					ods listing;

					%if %sysfunc(exist(pval)) %then %do;
						data pval;
						set pval;
						WHERE lowcase(method)='satterthwaite';
						length test_stat $4 test_stat_val 8 pval 8;
						test_stat="T";
						test_stat_val=tvalue;
						pval=probt;
						keep test_stat: pval;
						run;

						proc sort data=pval nodupkey; by test_stat; run;
						%let pval_done=1;
					%end;
				%end;
				%else %if &doMedian %then %do;
					ods output KruskalWallisTest=pval;
						proc npar1way data=pfile_temp;
						class &S1;
						var &V;
						run;
					ods output close;
					ods listing;

					%if %sysfunc(exist(pval)) %then %do;
						data pval;
						set pval;
						length test_stat $4 test_stat_val 8 pval 8;
						test_stat="KW";
						test_stat_val=chisquare;
						pval=prob;
						keep test_stat: pval;
						run;

						proc sort data=pval nodupkey; by test_stat; run;
						%let pval_done=1;
					%end;
				%end;
			%end;
			
			* check for missing values -- if present, add a count of missing values by 
			stratum at the bottom of the table ;
			proc sql noprint;
			select max(&V._nmiss) into :hasMiss from means_slice;
			quit;
			%if &hasMiss %then %do;
				%if not(&V in &misslist) %then %do;
					%let misslist=&misslist &V;
					%let vraw=&V:FMISSVAL;
					proc format;
					value FMISSVAL
						-999="&V._miss"
						other="&V._ok"
						;
					run;
				%end;
				%else %let hasMiss=0;
			%end;
			data means_slice;
			set means_slice;
			drop &V._nmiss;
			run;
		%end;

		** get variables that identify the levels of rowvar V<i> ;
		proc sql noprint;
		create table mslicevars as select name, varnum from dictionary.columns
		where libname='WORK' and lowcase(memname)='means_slice' and upcase(name) NOT in (&qsvarlist "_TYPE_" "_FREQ_")
		order by varnum;
		quit;

		** save the above into a single string and count the # of levels ;
		data _null_;
		set mslicevars END=LAST;
		length levvarstring $800;
		retain levvarstring '' nlevs 0;
		levvarstring=catx(' ',levvarstring,name);
		nlevs+1;
		if last then do;
			call symputx("vlevs",levvarstring);
			call symputx("nlevs",nlevs);
		end;
		run;

		%put VVVLEVS: &vlevs  (V: &V);

		* if this is the first row variable, establish column headers for each combination of stratifying levels ;
		%if &i=1 %then %do;
			data means_slice;
			length &svarlist $32;
			set means_slice END=LAST;
			length colvarstring colvarstring2 $1200 varorder $2400 squash squash2 $40 ncols 8;
			retain colvarstring '' colvarstring2 '' varorder '' ncols 0;
			array svars {*} &svarlist;
			do i=1 to dim(svars);
				svars[i]=coalesceC(svars[i],'ALL');
			end;
			squash=vvalue(svars[1]);
			if dim(svars)>1 then do;
				do i=2 to dim(svars);
					squash=compress(squash || '_' || vvalue(svars[i]));
				end;
			end;

			squash2=compress(squash || '_2');
			if length(squash2)>32 then do;
				put "ERROR: at least one combination of your stratifying variables produces a variable that is >32 char long!";
				put "..." squash2;
				abort cancel;
			end;
			colvarstring=catx(' ', colvarstring, squash);
			colvarstring2=catx(' ', colvarstring2, squash2);
			varorder=catx(' ', varorder, squash, squash2);
			ncols+1;
			if last then do;
				call symput("colvars",colvarstring);
				call symput("colvars2",colvarstring2);
				call symput("varorder",varorder);
				call symputx("ncols",ncols);
			end;
			run;
		%end;

		data means_slice (keep=var lvl &varorder has_missing test_stat test_stat_val pval);
		set means_slice END=LAST;
		length var lvl $32 &varorder 8 has_missing $1 test_stat $4 test_stat_val 8 pval $8;
		test_stat=''; test_stat_val=.; pval=.;
		array CL {&nlevs,&ncols} _temporary_;
		array F {&ncols} _temporary_;
		array cols {*} &colvars;
		array cols2 {*} &colvars2;
		array vo {*} &varorder;
		array levs {*} &vlevs;
		do i=1 to dim(CL,1);
			CL[i, _N_]=levs[i];  
		end;
		F[_N_]=_freq_;
		if last then do;
			var="&V";
			has_missing='';
			do i=1 to %if &vtype=S %then 1; %else dim(CL,1);;
				if &i=1 and i=1 then do;
					var='Pop';
					lvl=' ';
					do n=1 to dim(F);
						cols[n]=F[n];
						cols2[n]=F[n]/max(of F[*]);
					end;
					output;
					call missing(of cols2[*]);
					var="&V";
				end;
				if i=1 and "&vtype"="F" then do;
					if "&Vf"^="FMISSVAL" then do;
						call missing(of vo[*]);
						output;
						var=' ';
					end;
				end;
				if "&vtype"="F" then lvl=tranwrd(vname(levs[i]),compress('_' || scan(vname(levs[i]),-1,'_')),'');
				else if "&vtype"="D" then lvl=' ';
				else if "&vtype"="S" then do;
					lvl=scan(vname(levs[i]),-1,'_');
					if dim(levs)=2 then lvl=compbl(lvl || scan(vname(levs[2]),-1,'_'));
				end;
				do j=1 to dim(CL,2);
					if "&vtype" in ("F","D") then do;
						cols[j]=max(0,CL[i,j]);
						cols2[j]=max(0,cols[j]/F[j]);
					end;
					else do;
						cols[j]=CL[1,j];
						if dim(CL,1)=2 then cols2[j]=CL[2,j];
					end;
				end;
				if "&vtype" in ("F","S") then do;
					* do not include a 'missing' row if there were no missing values for <V> ;
					if "&vtype"="F" and ((lvl="_miss" and sum(of cols[*])=0) or lvl="&V._ok") then continue;
					if lvl in ("&V._miss","_miss") then lvl='(missing)';
					if &hasMiss then do;
						call symputx("anymissingvals",1);
						has_missing='Y';
					end;
					output;
				end;
				else if i=2 then output;  /* i=2 is the <TRUE> record when <V> is a dummy var (vtype=D) */
			end;
		end;
		run;

		data means_slice;
		length vtype $1;
		set means_slice;
		vtype="&vtype";
		run;

		%if &pval_done %then %do;
			data _null_;
			set pval;
			call symputx("teststat",test_stat);
			call symputx("teststatval",test_stat_val);
			call symputx("pval",pval);
			run;

			data means_slice;
			set means_slice;
			length pdone 3;
			retain pdone 0;
			if not pdone then do;
				if var^='Pop' then do;
					if not missing(var) then do;
						test_stat="&teststat";
						test_stat_val=&teststatval;
						if &pval<0.0001 then pval='<0.0001';
						else pval=put(&pval,8.4);
						pval=right(pval);
						pdone=1;
					end;
				end;
			end;
			drop pdone;
			run;

			proc datasets lib=work memtype=data nolist nodetails; delete pval; run; quit;
			%let teststat=;
			%let teststatval=;
			%let pval=;
		%end;

		%if &hasheaderrow %then %do;

			%do _b=1 %to %eval(&doubleblank+1);
				data _blank;
				set means_slice (obs=1);
				array c {*} _character_;
				array n {*} _numeric_;
				call missing(of c[*], of n[*]);
				if &_b=&doubleblank+1 then var="&header";
				run;

				proc append data=_blank base=table1; run;
			%end;

			proc datasets lib=work memtype=data nolist nodetails; delete _blank; run; quit;

			%let hasheaderrow=0;
			%let header=;

		%end;

		data means_slice;
		set means_slice;
		if var^='Pop' and not missing(var) then var="&vrawlabel";
		run; 

		proc append data=means_slice base=table1; run;
		
		%if &hasMiss %then %do;
			%let isRedo=1;
			%goto redo;
		%end;

		%nextvar:

	%end; *nvars loop;

	%if &pvalues=0 %then %do;
		data table1;
		set table1;
		drop test_stat test_stat_val pval;
		run;
	%end;
	
	%if &anymissingvals=0 %then %do;
		data table1;
		set table1;
		drop has_missing;
		run;
	%end;
	%if &IMvars^= %then %do;
		%let IMdlm="%scan(&IMvars,1,' ')";
		%let nim=%sysfunc(countW(&IMvars,' '));
		%if &nim>1 %then %do;
			%do i=2 %to &nim;
				%let IMdlm=&IMdlm, "%scan(&IMvars,&i,' ')";
			%end;
		%end;

		data table1;
		set table1;
		array V {&nim} $32 _temporary_ (&IMdlm);
		array N {&ncols} _temporary_; 
		array M {&ncols} _temporary_;
		array cols {*} &colvars;
		array cols2 {*} &colvars2;
		length vn $32;
		retain vn;
		if var='Pop' then do;
			do i=1 to dim(N);
				N[i]=cols[i];
			end;
		end;
		if not missing(var) then do;
			vn=var;
			do i=1 to dim(M);
				M[i]=0;
			end;
		end;
		if lvl='(missing)' then do;
			do i=1 to dim(M);
				M[i]=cols[i];
			end;
			if compress(vn) in V then lvl='(none)';
		end;
		if compress(vn) in V and lvl not in ('(missing)','(none)') and not missing(lvl) then do;
			do i=1 to dim(N);
				if cols2[i]>. then cols2[i]=cols[i]/(N[i]-M[i]);  * i.e., denom for percentages in COL2 is now limited to non-missing values for <VAR> ;
			end;
		end;
		drop i vn;
		run;
	%end;
	
	data table1; set table1; drop vtype; run;
	
	%if &printSMD = 1 %then %do;
		/* 
		SMD CODE STARTS HERE -- the following is adapted from Ethan Powells SMD algorithm / macros.
 
		-- if row contains means and standard deviations...
			* calculate SMD as 100*(Xt-Xc) / sqrt( (St**2 + Sc**2)/2 ) ...
				where Xt and Xc are the mean values of treatment and control groups, respectively
				and
				St and Sc are the standard deviations of treatment and control groups, respectively
		-- if row contains counts and percentages...
			* calculate SMD as 100*(Pt-Pc) / sqrt( (Pt*(1-Pt) + Pc*(1-Pc))/2 ) ...
				where Pt and Pc are the proportions (percentages) of treatment and control groups, respectively */
		data table1;
		set table1;
		array p {0:2} &colvars;		* primary statistics ;
		array s {0:2} &colvars2;	* secondary statistics ;
		if lowcase(lvl)='mean stddev' then SMD = 100*(p[2] - p[1]) / sqrt((s[2]**2 + s[1]**2)/2);
		else if cmiss(var,lvl)=1 or lvl in ('(missing)','(none)') then SMD = 100*(s[2] - s[1]) / sqrt((s[2]*(1-s[2]) + s[1]*(1-s[1]))/2);
		run;
	%end;

%MEND; *table1();
