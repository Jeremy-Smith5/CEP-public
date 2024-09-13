
/*
	Calculate a bootstrapped difference in mean or median <INVAR> between 2 levels
	of a given <BYVAR> and return a dataset containing an overall mean or median 
	and 95% confidence interval for these differences.  Optionally, create plots
	of the above.

	J Smith
	Jul 2024

	NOTE: sample call at bottom 
	
	======================================================================= */

%macro getCI(var);

	proc sort data=all_m; by &var; run;

	data 
		all_m (drop=l95_&var &boottype._&var u95_&var)
		CI_&var (keep=l95_&var &boottype._&var u95_&var)
		;
	set all_m;
	length l95_&var &boottype._&var u95_&var 8;
	retain l95_&var &boottype._&var u95_&var;
	if _N_=&lb then l95_&var=&var;
	if _N_=&mid then &boottype._&var=&var;
	if _N_=&ub then do;
		u95_&var=&var;
		output CI_&var;
	end;
	output all_m;
	run;

	data all_m;
	set all_m;
	if _N_=1 then set CI_&var;
	run;

%mend; *getCI();

%MACRO bootmdiff (
	boottype=median,	/* options: MEAN or MEDIAN */
	indata=, 	/* input SAS dataset */
	ptid=,		/* variable representing the patient identifier */
	invar=, 	/* numeric variable for which to estimate median/mean and CI */
	invar_lab=,	/* optional - if provided, will be used in plot title to represent <INVAR> */
	byvar=, 	/* categorical variable to differentiate one set of <INVAR> values from another */
	byvar_lab=,	/* optional - if provided, will be used in plot title to represent <BYVAR> */
	byvar_lev0=, 	/* level of <BYVAR> corresponding to unexposed */
	byvar_lev1=, 	/* level of <BYVAR> corresponding to exposed */
	reps=1000, 	/* number of repetitions for bootstrap -- min: 1000 */
	mkplot=0,	/* make a histogram */
	textfmt=	/* format to apply to <INVAR>, e.g., dollar8., 3.1, percent8.1 */
	); /* output: result in .lst printout, WORK.all_m, plots (optional) */

	proc datasets lib=work memtype=data nolist nodetails; delete all_m; run; quit;

	%let boottype=%lowcase(&boottype);
	%if &boottype^=median and &boottype^=mean %then %do;
		%put ::: boottype options are MEAN or MEDIAN! ;
		%abort cancel;
	%end;
	
	%if &reps<1000 %then %do;
		%put ::: set reps to at least 1000! ;
		%abort cancel;
	%end;
	%if %length(&textfmt) %then %do;
		%if %index(&textfmt,.)=0 %then %do;
			%let textfmt=&textfmt..;
		%end;
	%end;
	%if %length(&invar_lab)=0 %then %do;
		%let invar_lab=&invar;
	%end;
	%if %length(&byvar_lab)=0 %then %do;
		%let byvar_lab=by &byvar value;
	%end;

	data _null_;
	call symputx("lb",int(&reps*0.025));
	call symputx("mid",int(&reps*0.5));
	call symputx("ub",int(&reps*0.975));
	run;

	%let indataname=%upcase(&indata);
	%let inlib=WORK;
	%if %index(&indataname,.) %then %do;
		%let inlib=%scan(&indataname,1,.);
		%let indataname=%scan(&indataname,2,.);
	%end;

	proc format;
	value $fd
		'num'=''
		'char'='$'
		;
	run;

	proc sql noprint;
	select compress(put(type,$fd.) || put(length,8.)) into :ptid_len trimmed
	from dictionary.columns where libname="&inlib" and upcase(memname)="&indataname"
	and upcase(name)=upcase("&ptid");

	select count(*) into :npts trimmed from &indata;
	quit;

	%let type=N;
	%if %index(&ptid_len,$) %then %let type=C;;

	data num2pt;
	length fmtname $8 type $1 start 8;
	set &indata (keep=&ptid rename=(&ptid=label));
	retain fmtname 'fnum2pt' type 'N';
	start=_N_;
	run;

	proc format cntlin=num2pt; run;
		
	%do r=1 %to &reps;

		data samp (keep=&ptid);
		call streaminit(&r);
		retain total_out 0;
		length &ptid &ptid_len;
		do while (total_out<&npts);
			&ptid=put(int(rand('uniform')*&npts+1),fnum2pt.) %if &type=N %then *1;;
			total_out+1;
			output;
		end;
		run;

		proc sql;
		create table samp as
		select a.&ptid, a.&invar, a.&byvar
		from
			&indata A
			inner join
			samp B
			on a.&ptid=b.&ptid;
		quit;

		proc means data=samp noprint;
		class &byvar;
		var &invar;
		output out=M &boottype=m / autoname;
		run;

		data M;
		length rep lev0 lev1 mdiff 8;
		set M end=last;
		retain lev0 lev1;
		rep=&r;
		if &byvar=&byvar_lev0 then lev0=m;
		if &byvar=&byvar_lev1 then lev1=m;
		if last then do;
			mdiff=lev1-lev0;
			output;
		end;
		keep rep lev0 lev1 mdiff;
		run;

		proc append data=m base=all_m; run;

		proc datasets lib=work memtype=data nolist nodetails; delete samp m; run; quit;

	%end;

	%getCI(mdiff);
	%getCI(lev0);
	%getCI(lev1);

	data all_m;
	set all_m;
	one='.'; * this is just used to trick SGPANEL into allowing a single plot ;
	sep='-';
	if min(of l95:)<0 then sep=',';
	mdiff_CI=compbl(put(&boottype._mdiff,&textfmt) || ' ' || compress('(' || put(l95_mdiff,&textfmt) || sep || put(u95_mdiff,&textfmt) || ')'));
	lev0_CI=compbl(put(&boottype._lev0,&textfmt) || ' ' || compress('(' || put(l95_lev0,&textfmt) || sep || put(u95_lev0,&textfmt) || ')'));
	lev1_CI=compbl(put(&boottype._lev1,&textfmt) || ' ' || compress('(' || put(l95_lev1,&textfmt) || sep || put(u95_lev1,&textfmt) || ')'));
	label 
		mdiff_CI="%sysfunc(propcase(&boottype)) difference (95% CI)"
		lev0_CI="%sysfunc(propcase(&boottype)) unexposed (95% CI)"
		lev1_CI="%sysfunc(propcase(&boottype)) exposed (95% CI)"
		;
	run;

	title "difference in &boottype &invar: &byvar=&byvar_lev1 vs. &byvar_lev0";
	proc print data=all_m (obs=1) noobs; var mdiff_CI lev0_CI lev1_CI; run;
	title;

	%if &mkplot %then %do;

		%include '/data/prod/common/WRJ_macros/plot_syntax.sas';

		%plot_syntax(
			outgraph_imagefmt=tiff,  	/* extension: tiff, html, png, jpeg -- pdf may work also */
			outgraph=&invar._diff_&boottype._by_&byvar, 		/* name for output image (no extension) */
			gwidth=12,		/* graph width in inches */
			gheight=8,		/* graph height in inches */
			graphdir=&CWD,		/* output path for graph */
			orientation=landscape,	/* portrait or landscape */
			otheroptions=,		/* enclose in %str( ), e.g., %str(dataskinmax=1000 antialiasmax=1000) - no semicolons */
			listingoptions=,	/* enclose in %str( ) -- options to be added to ODS LISTING statement, if any */
			outgraph_title=%str(Difference in &invar_lab &byvar_lab - bootstrapped &boottype and CI),  /* title -- can be left blank */
				outgraph_title_font=Times New Roman,	/* font for title */
				outgraph_title_height=8pt		/* font size for title */
			);

			footnote height=8pt italic justify=right "Bootstrapping: repetitions (&reps)";
 
			* using sgpanel instead of sgplot d/t incompatibility of HISTOGRAM and TEXT statements in sgplot ;
			proc sgpanel data=all_m NOAUTOLEGEND;
			format mdiff &boottype._mdiff l95_mdiff u95_mdiff &textfmt;
			panelby one / novarname onepanel columns=1 noheaderborder border uniscale=all headerattrs=(color=white);
			histogram mdiff / dataskin=pressed ;
			refline l95_mdiff / axis=x lineattrs=(color=red pattern=shortdash thickness=2);
			refline &boottype._mdiff / axis=x lineattrs=(color=red pattern=solid thickness=2);
			refline u95_mdiff / axis=x lineattrs=(color=red pattern=shortdash thickness=2);
			inset mdiff_CI / textattrs=(size=10 weight=bold) position=topright separator=':' backcolor=lightgray border;
			colaxis label="difference in &invar_lab" valuesformat=&textfmt;
			run;

			title;
		
		ods listing close;
		ods listing;

		data lev0lev1 (keep=rep level lev one lev0_CI lev1_CI l95_lev: &boottype._lev: u95_lev:);
		length level $4;
		set all_m;
		one='.';
		lev=lev0;
		level='Lev0';
		output;
		lev=lev1;
		level='Lev1';
		output;
		run;

		%plot_syntax(
			outgraph_imagefmt=tiff,  	/* extension: tiff, html, png, jpeg -- pdf may work also */
			outgraph=&invar._ovrlay_&boottype._by_&byvar, 		/* name for output image (no extension) */
			gwidth=12,		/* graph width in inches */
			gheight=8,		/* graph height in inches */
			graphdir=&CWD,		/* output path for graph */
			orientation=landscape,	/* portrait or landscape */
			otheroptions=,		/* enclose in %str( ), e.g., %str(dataskinmax=1000 antialiasmax=1000) - no semicolons */
			listingoptions=,	/* enclose in %str( ) -- options to be added to ODS LISTING statement, if any */
			outgraph_title=%str(Distribution of &invar_lab &byvar_lab - bootstrapped &boottype and CI),
				outgraph_title_font=Times New Roman,	/* font for title */
				outgraph_title_height=8pt		/* font size for title */
			);

			* using sgpanel instead of sgplot d/t incompatibility of HISTOGRAM and TEXT statements in sgplot ;
			proc sgpanel data=lev0lev1 NOAUTOLEGEND;
			format lev &boottype._lev: l95_lev: u95_lev: &textfmt;
			panelby one / novarname onepanel columns=1 noheaderborder border uniscale=all headerattrs=(color=white);
			histogram lev / group=level dataskin=pressed ;
			refline l95_lev0 / axis=x lineattrs=(color=blue pattern=shortdash thickness=2);
			refline &boottype._lev0 / axis=x lineattrs=(color=blue pattern=solid thickness=2);
			refline u95_lev0 / axis=x lineattrs=(color=blue pattern=shortdash thickness=2);
			
			refline l95_lev1 / axis=x lineattrs=(color=red pattern=shortdash thickness=2);
			refline &boottype._lev1 / axis=x lineattrs=(color=red pattern=solid thickness=2);
			refline u95_lev1 / axis=x lineattrs=(color=red pattern=shortdash thickness=2);

			inset lev0_CI lev1_CI / textattrs=(size=10 weight=bold) position=topright separator=':' backcolor=lightgray border;
			colaxis label="&invar_lab &byvar_lab" valuesformat=&textfmt;
			run;

			title; footnote;
		
		ods listing close;
		ods listing;

	%end;

	proc datasets lib=work memtype=data nolist nodetails; delete lev:; run; quit;

%MEND; *bootmed();


/* 
* sample call -- uncomment to run ;

* sim data for testing ;
data test;
length admit pandemic LOS 5;
format admit date9.;
do i=1 to 100000;
	admit=int(ranuni(0)*1500)+'01Jan2017'd;
	pandemic=(admit>='01Mar2020'd);
	LOS=int(ranuni(0)*12)+1+int(ranuni(0)*3*pandemic);
	output;
end;
drop i;
run;

%bootmdiff(
	indata=test, 
	invar=LOS, 
	byvar=pandemic, 
	byvar_lev0=0, 
	byvar_lev1=1, 
	reps=1000, 
	);
*/






