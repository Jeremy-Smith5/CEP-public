
/*
	Given an input dataset and a list of numbers corresponding
	to attrition flags of interest, create a dataset where other 
	flag variables (those not represented in the list of numbers)
	are renamed with an underscore prefix.

	Optionally:
		- show cohort flow
		- subset to specified population

	Output dataset: WORK.subpop OR WORK.subpop_&poplab

	J Smith
	June 2024

	NOTE: flag prefix can be specified - however, the variable
	name must match this format: <flagprefix>#[letter]_[suffix], e.g., 
	flag03_isVeteran, flag3_isVeteran, flag03b_isVeteran etc. - an underscore 
	must come immediately after the number or number/letter except in the case
	of an exclusion variable, which must be in the format:
	<flagprefix>#x_[suffix], e.g. flag02x_under18.

	<flagnums>, if specified, does not need to be in numerical order - 

	** attrition will be carried out in the order provided **

	If <flagnums> is not specified, all flags will be used.

	This macro will automatically EXIT 2 if any flag variables
	have any value other than 0/1.

	EXAMPLE of $fpar format to be supplied when OFFSET_FLOW=1:
		proc format;
		value $fpar
			'2'='base'  i.e., flag02 depends on the base population, not on flag 01
			'6a'='4'  i.e., flag06a depends on flag04
			'8a'='7'
			'8b'='7'
			'13'='10b'
			;
		run;
		
		Flags not specified in $fpar will depend on the 
		flag immediately previous in the list (as usual). 
		For example, if FLAGNUMS=1 2 3 5 4 6 6a 7 8 8a 8b 12 10 10b 11 13
		and $fpar is specified as above, flag 12 will depend 
		on 8b while 8b will depend on 7.

	UPDATES:
		- 25 Mar 2025 
			- added checks to crash program if duplicate
			or non-existent flags are provided in <flagnums>
		

	============================================================ */


%MACRO subpop(
	indata=,		/* can be permanent - must contain (at least) patientICN, <flagprefix>: */
	flagnums=,		/* a list of numbers, e.g., 1 2 3 5 8 10 that correspond to flags of interest - leading zeros are NOT required */
	offset_flow=0,		/* allows branching (i.e., flag is dependent on a flag earlier than the prior) - if set to 1, 
					subset will be forced to 0 (no subsetting done) and user must supply a format called $fpar 
					that crosswalks offset flags to their immediate parent */
	rename_others=0,	/* 0: leave other flags (not identified by <flagnums>) w. original name ... 1: rename these by prefixing with '_' */	
	flagprefix=flag,	/* the entire prefix that precedes the flag number */
	poplab=,		/* optional - if provided, this will be added as a suffix to the output dataset name, e.g., WORK.subpop_age65plus */
	showflow=1,		/* 0: do not show cohort flow ... 1: show cohort flow output - NOTE: if using offset_flow, this is automatically 1 */
	showflow_plot=0,	/* currently not possible if using offset_flow */
	subset=1		/* 0: return WORK.subpop_&poplab w/o subsetting ... 1: return WORK.subpop_&poplab limited to records where all
					flags identified by <flagnums> equal 1 (or zero in the case of exclusion variables) NOTE: 
					subsetting is currently not possible if using offset_flow (i.e., flow with branching) */		
	); /* OUTPUT: WORK.subset or WORK.subset_&poplab AND global macro variable SPFLAGLIST containing names of selected variables */

	%let flagprefix=%upcase(&flagprefix);
	%let flagprefix=%scan(&flagprefix,1,:);
	%let prelen=%length(&flagprefix);

	%let spindata=%upcase(&indata);
	%let spinlib=WORK;
	%if %index(&spindata,.) %then %do;
		%let spinlib=%scan(&spindata,1,.);
		%let spindata=%scan(&spindata,2,.);
	%end;

	%if &offset_flow %then %do;
		proc sql noprint;
		select count(*) into :has_parfmt trimmed 
		from dictionary.formats where lowcase(fmtname)='$fpar';
		quit;

		%if &has_parfmt=0 %then %do;
			%put ::: You must supply the format $fpar as specified in the header for the SUBPOP macro! ;
			%abort cancel;
		%end;

		%if &subset %then %do;
			%put ::: WARNING: offset_flow was set to 1, so SUBSET was reset to 0 - no subsetting done! ;
		%end;
		%let subset=0;
	%end;

	proc sql undo_policy=NONE noprint;
	create table flagvars as
	select name from dictionary.columns
	WHERE libname="&spinlib" and upcase(memname)="&spindata" and upcase(substr(name,1,&prelen))="&flagprefix"
	order by name;
	
	select count(*) into :totflagvars trimmed from flagvars;
	quit;

	%if %length(&flagnums)=0 %then %do;
		data _null_;
		set flagvars end=last;
		array T {&totflagvars} $6 _temporary_;
		T[_N_]=substr(name,length("&flagprefix")+1,index(name,"_")-length("&flagprefix")-1);
		if last then call symputx("flagnums",catx(' ', of T[*]));
		run;
	%end;

	%let flagnums=%lowcase(%cmpres(&flagnums));
	%let flagnums_csv="%scan(&flagnums,1,' ')";
	%let nfl=%sysfunc(countW(&flagnums,' '));
	%let dupflag=0;
	%if &nfl>1 %then %do;
		%do i=2 %to &nfl;
			%let nextflag=%scan(&flagnums,&i,' ');
			data _null_;
			if "&nextflag" in (&flagnums_csv) then call symputx("dupflag",1);
			run;
			%let flagnums_csv=&flagnums_csv, "&nextflag";
		%end;
	%end;

	%if &dupflag %then %do;
		%put ::: ERROR: at least one flag in (&flagnums) is repeated - SAS is quitting ;
		%abort cancel;
	%end;

	%let spoutdata=subpop;
	%if %length(&poplab) %then %do;
		%let spoutdata=subpop_&poplab;
	%end;

	%let exit=0;

	%global spflaglist;
	%let spflaglist=;
	%let sprnlist=;

	data 
		flagvars&poplab (keep=sortorder joinstring step) /* this dataset gets used by the flow_overlap macro */
		num2pos (keep=fmtname type start label)
		;
	set flagvars end=last;
	array oth {&totflagvars} $65 _temporary_;
	array T {&nfl} $32 _temporary_;
	array Tpos {&nfl} _temporary_;
	array Tjoin {&nfl} $32 _temporary_;
	array isX {&nfl} _temporary_;
	array Tf {&nfl} $5 _temporary_ (&flagnums_csv);
	array valid {&nfl} 3 _temporary_ (&nfl*0);
	retain ov 0 dupflag 0;
	upos=index(name,'_');
	* allow the possibility of exclusion flags (with 'x' between # and '_'), e.g., flag02x_under18 ;
	fx=0;
	if lowcase(substr(name,upos-1,1))='x' then do;
		upos=upos-1;
		fx=1;
	end;
	fn=lowcase(substr(name,&prelen+1,upos-(&prelen+1)));
	if anydigit(fn)^=1 then do;
		call symputx("exit",1);
		call symputx("exitvar",name);
		stop;
	end;
	fn_end=length(fn);
	if anyalpha(fn) then fn_end=anyalpha(fn);
	i=1;
	do while (i<=fn_end-1);
		if substr(fn,i,1)^='0' then leave;
		i+1;
	end;
	fn_actual=substr(fn,i);
	foundname=0;
	do i=1 to dim(Tf);
		if Tf[i]=fn or Tf[i]=fn_actual then do;
			foundname=1;
			T[i]=name;
			Tjoin[i]=compress("&flagprefix" || fn);
			isX[i]=fx;
			if valid[i] then dupflag=1;
			valid[i]=1;
			leave;
		end;
	end;
	if not foundname then do;
		ov+1;
		oth[ov]=compress(name || '=_' || name);
	end;
	if last then do;
		call symputx("spflaglist",catx(' ', of T[*]));
		length fmtname $8 type $1 start $8 label 3;
		fmtname='fnum2pos'; type='C';
		do label=1 to dim(Tf);
			start=Tf[label];
			output num2pos;
		end;
		call symputx("isX",catx(',', of isX[*]));
		call symputx("nff",dim(isX)-nmiss(of isX[*]));
		call symputx("dupflag",dupflag); * indicates that a flag in <flagnums> was repeated ;
		call symputx("all_valid",min(of valid[*])); * all_valid=0 indicates at least 1 flag in <flagnums> does not exist in the data ;
		if ov then call symputx("sprnlist",catx(' ',of oth[*]));
		length sortorder 3 joinstring step $32;
		do sortorder=1 to dim(Tjoin);
			joinstring=Tjoin[sortorder];
			step=T[sortorder];
			output flagvars&poplab;
		end;
	end;  
	run;

	%if &all_valid=0 %then %do;
		%put ::: ERROR: at least one of these flags (&flagnums) does not exist in the data - SAS is quitting ;
		%abort cancel;
	%end;

	data base;
	length fmtname $8 type $1 start $8 label 3;
	fmtname='fnum2pos'; type='C'; start='base'; label=0;
	run;

	data num2pos;
	set
		base
		num2pos
		;
	run;

	proc format cntlin=num2pos; run;

	%if &exit %then %do;
		%put ::: the variable &exitvar is not valid for this macro - SAS is quitting ;
		%abort cancel;
	%end;

	%put ::: dataset WORK.&spoutdata -- using flags: &spflaglist;
	%if %length(&sprnlist) AND &rename_others %then %do;
		%put ::: ...other flag variables will be re-named as follows: &sprnlist;
	%end;
	%else %if &rename_others %then %do;
		%put ::: ALERT: there were no other flag variables found aside from &flagnums! ;
	%end;

	data WORK.&spoutdata;
	set &indata %if %length(&sprnlist) AND &rename_others %then %do; (rename=(&sprnlist)) %end;;
	array spF {*} &spflaglist;
	do i=1 to dim(spF);
		if spF[i] not in (0,1) then do;
			call symputx("exit",1);
			stop;
		end;
	end;
	drop i;
	run;

	%if &exit %then %do;
		%put ::: at least one flag variable contains non-0/1 values - SAS is quitting! ;
		%abort cancel;
	%end;
	
	%if &offset_flow %then %do;

		data _null_;
		array T {&nff} $8 _temporary_ (&flagnums_csv);
		length lastoffset 3 lastoffsetflag $8;
		lastoffset=0;
		do i=1 to dim(T);
			if put(T[i],$fpar.)^=T[i] then do;
				lastoffset=i;
				lastoffsetflag=T[i];
			end;
		end;
		call symputx("lastoffset",lastoffset);
		call symputx("lastoffsetflag",lastoffsetflag);
		run;

		%put ::: unconditional checks stop at flag position: &lastoffset flag: &lastoffsetflag;
		%put ::: # flag vars found: &nff;

		* make an attrition table according to the format $fpar ;
		data flow (keep=parent step before removed);
		set WORK.&spoutdata end=last;
		length _base 3 parent $8;
		_base=1;
		array vn {0:&nff} _base &spflaglist;
		array isX {0:&nff} _temporary_ (0,&isX);
		array T {&nff} $8 _temporary_ (&flagnums_csv);
		array C {&nff} _temporary_;
		array B {0:&nff} _temporary_;
		array OK {0:&nff} _temporary_;
		call missing(of OK[*]);
		OK[0]=1;
		do i=1 to &nff;
			parent=put(T[i],$fpar.);
			if lowcase(parent)=lowcase(T[i]) then ppos=i-1;
			else ppos=put(parent,$fnum2pos.)*1;
			OK[i]=((OK[ppos]^=isX[ppos]) * (vn[i]^=isX[i]));  * i.e., parent is OK and current flag (i) is OK ;
			if OK[i] then C[i]+1;
			else if i>=&lastoffset then leave;
		end;
		B[0]+1;
		if last then do;
			length step $32 before removed 8;
			do i=1 to &nff;
				parent=put(T[i],$fpar.);
				if lowcase(parent)=lowcase(T[i]) then do;
					parent=' ';
					ppos=i-1;
				end;
				else ppos=put(parent,$fnum2pos.)*1;
				step=coalesceC(vlabel(vn[i]), vname(vn[i]));
				before=B[ppos];
				removed=before-C[i];
				output flow;
				B[i]=before-removed;
			end;
		end;
		run;

		title "cohort flow for &poplab";
		proc print data=flow noobs width=min; run;

		%goto stopSP;

	%end;

	%if &showflow %then %do;

		%include '/data/prod/common/WRJ_macros/cohort_flow.sas';

		%let showflow_plot_dir=;
		%if &showflow_plot %then %let showflow_plot_dir=&CWD;;

		%flow(
			srcdat=WORK.&spoutdata,
			srcdat_descrip=&poplab,
			flaglist=&spflaglist,
			flagvals=,
			squash=1,
			order_by_alpha=1,	/* if 1, order flags in <FLAGLIST> alphabetically ... if 0, use dataset var order (if no 
						colon was used in <FLAGLIST>, just use order as stated in list) */
			allowFlagX=1,
			label_len=120,
			out_rscript=,
				rscript_incl_vname=1,
				rscript_png_prefix=,
			printflow=0,
			out_SASplot=&showflow_plot,	
				rows_per_column=6,
				
				outgraph_imagefmt=tiff,  	/* extension: tiff, html, png, jpeg -- pdf may work also */
				outgraph=flow_&spoutdata, 		/* name for output image (no extension) */
				gwidth=12,		/* graph width in inches */
				gheight=9,		/* graph height in inches */
				graphdir=&showflow_plot_dir,		/* output path for graph, e.g., &PDRIVE */
				orientation=landscape,	/* portrait or landscape */
				outgraph_title=Cohort Attrition,		/* title -- can be left blank */
					outgraph_title_font=Times New Roman,	/* font for title */
					outgraph_title_height=8pt		/* font size for title */
			);

	%end;

	%if &subset %then %do;	

		data WORK.&spoutdata;
		length subpop $32;
		set WORK.&spoutdata;
		subpop="&poplab";
		array spF {*} &spflaglist;
		array isX {&nff} _temporary_ (&isX);
		do i=1 to dim(spF);
			if isX[i] then do;
				* this is an exclusion flag - drop record if flag=1 ;
				if spF[i]=1 then do; 
					delete;
					leave;
				end;
			end;
			else do;
				* this is an inclusion flag - drop record if flag=0 ;
				if spF[i]=0 then do;
					delete;
					leave;
				end;
			end;
		end;
		drop i;
		run;

		proc sql undo_policy=NONE noprint;
		select count(*) into :sprecs trimmed from WORK.&spoutdata;
		quit;

		%put ::: SubPop Records for WORK.&spoutdata: &sprecs;

	%end;

	%stopSP:

%MEND; *subpop();

