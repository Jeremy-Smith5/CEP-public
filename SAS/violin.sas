/*
	For a given input dataset containing a numeric variable and a grouping variable, create a plot containing 
	a 'violin'-style kernel density estimate distribution of the numeric variable for each level of the grouping 
	variable.  

	NOTES:
		- the 'quantbands' option is based on the idea of a SAS user named James Marcus described here:
			https://blogs.sas.com/content/graphicallyspeaking/2012/10/30/violin-plots/  
		This macro does not use code from elsewhere but does use the color pallette for the quantile bands
		as shown in the link above: 
			extremes: cxBDD7E7 (very light grayish blue)
			5th-<25th and >75th-95th percentiles: cx6BAED6 (light blue)
			25th-75th percentile: cx3182BD (dark blue) 

			NOTE: the quantbands option currently does not work well for data that have abrupt changes 
				in distribution (sharp curves) and also currently does not have sufficient color
				options added to work in situations with more than one group variable. Patterns 
				can be combined with colors to distinguish groups within groups.

		- to be added:
			- option to sort violins based on median value (currently, violins are automatically sorted based on 
				alphabetic or numeric value of the grouping variable)

	Jeremy Smith
	Oct 2024

	===================================================================================================================== */
	
%macro violin(
	indata=, /* input dataset - must contain at least DENSVAR and GRPVARS */
	outplot=, /* name of output plot file - do not include extension (extension will always be .tiff) */
	outplot_title=, /* optional - if contains special characters, enclose in %str( ) - no quotes */
	outplot_path=&CWD,  /* destination directory for OUTPLOT */
	denstype=KDE, /* options: KDE (kernel density estimate) or HISTO (makes plot based on actual binned values - this will be
				potentially much more granular and jagged than the plots produced by KDE) - if specifying histo,
				then also specify a value for HISTOBINSIZE (histogram bin size) below */
	histobinsize=1, /* only applicable if DENSTYPE is set to HISTO */
	densvar=, /* continuous or count variable for which distribution is being assessed */ 
	densLAB=,  /* label for value axis of plot that describes DENSVAR - if missing, defaults to DENSVAR - enclose in %str( ) */ 
	densRNG=, /* values to display on <densvar> axis, e.g., 0 to 100 by 5 - can be left blank */
	dens_keepflag=, /* if specified, a 0/1 flag variable that indicates whether an observation should be reflected in the plot itself - 
			all observations, regardless of 0/1 value, will be used in calculating counts, means and medians */
	grpvars=,  /* categorical variable used to group data into separate violins - can be character or numeric */
	grpfmts=,  /* formats to apply to grpvars - if specifying any, number listed must equal number of GRPVARS - group formats #2
			and higher will be concatenated into subgroups distinguished by FILLCOLORS and FILLPATTERNS.  Formats can
			be SAS built-ins or user-defined.  NOTE: if >1 group variable is listed and no formats are specified, then 
			all GRPVARS must be of the same type (char or numeric). */
	show_counts=1, /* displays N beneath each violin - if Ns are large and there are many violins, this may cause formatting issues */
	comparisons=, /* optional -- if specified, this should either be ALL or formatted like '3v1 2v1' to obtain mean or median 
			differences for the 3rd vs. the 1st and 2nd vs. the 1st distributions, respectively (according to order of violins) */
	comparison_type=,  /* MEAN or MEDIAN */
	comparison_fmt=,	/* numeric format to apply to difference estimate / CI, e.g., 8.1, dollar8., etc. */
	comparison_descrip=0,	/* 0: just show difference estimate / CI ... 1: add label, e.g., 'med diff (3 vs 1)', before est/CI */
	comparison_calc_only=0, /* 1: run comparisons but do not add to plot (results will be in the .lst file) */ 
	fillcolors=cxBDD7E7, /* options: NONE, AUTO, <color name> e.g. lightgrey, <color code> - NOTE:
			if option QUANTBANDS is set to 1, then fillcolor establishes the color for the extremes
			of the plot -- <5th percentile and >95th percentile - recommend either using 
			fillcolor=cxBDD7E7 (a light grayish blue) to accompany the other blue tones of the
			quantile bands OR WHITE, which will effectively make the extremes invisible */
	fillpatterns=, /* same as above, but using patterns - patterns and colors can be used together - these are standard SAS patterns, e.g. X1  */
	outline=1,	/* if set to 1, includes a thin black outline around each violin */
	quantbands=0, /* if set to 1, violin plots will denote 25th-75th percentile as dark blue, 5th-<25th and >75th-95th 
			percentiles as lighter blue, and the extremes as the color designated by FILLCOLOR (defaults to cxBDD7E7) */
	useCI95=0, /* only applicable if quantbands=1 -- 1: replace 5th and 95th percentiles with 2.5th and 97.5th instead */
	vertical=1, /* 0: violins will be horizontal; 1: violins will be vertical - note: regardless, ORIENT can be set to portrait or landscape */
	orient=,  /* optional -- landscape or portrait - if blank, defaults to 
			landscape if vertical=0 and portrait if vertical=1 */
	gwidth=,  /* optional -- an integer specifying width of plot - if blank,
			defaults to 12 for landscape or 8 for vertical */
	gheight=,  /* optional -- an integer specifiying height of plot - if blank,
			defaults to 8 for landscape or 12 for vertical */
	footnote=%str(),  /* optional: footnote to be added at bottom right */
	legend_loc_valaxis=,	/* optional (only for >1 grpvars) - actual value of DENSVAR at which to place upper left corner of interior legend */
	legend_loc_labaxis=,	/* optional (only for >1 grpvars) - violin pos  at which to place upper left corner of interior legend 
					- use whole numbers for directly above / below violin #N or decimal number to place between */
	valaxis_refline_statement=, /* e.g., %str(refline 0 / lineattrs=(color=blue pattern=shortdash thickness=1)) -- do NOT include AXIS or semicolon */
	labaxis_offset=0.02  /* amount of extra space to allow on either side of the first and last violin, as a fraction of the 
			overall axis range - this is somewhat trial and error.  If the plot you get is truncated on either side,
			try setting this to a higher number - generally the more violins are getting created, the smaller this 
			number should be - for example, if only generating violins for 5 groups, try 0.1. NOTE: If only one value
			is specified here, that amount will be applied to both the min and max of the LAB axis.  If two values 
			are specified, the first will be applied to the min side, and the 2nd to the max side. In general, the only
			reason here to specify two separate offsets would be if placing the legend on the far left or right of the plot. */
	);

	%let worktabs=;

	proc sql undo_policy=NONE noprint;
	select memname into :worktabs separated by ' '
	from dictionary.tables
	where libname='WORK';
	quit;

	%let denstype=%upcase(&denstype);
	%if %length(&denstype)=0 %then %let denstype=KDE;;
	%if &denstype^=KDE and &denstype^=HISTO %then %let denstype=KDE;;
	%if %length(&histobinsize)=0 %then %let histobinsize=1;;

	%let comparison_type=%upcase(&comparison_type);

	%if %length(&vertical)=0 %then %let vertical=1;;
	%if &quantbands and %length(fillcolors)=0 %then %do;
		%let fillcolors=cxBDD7E7;
	%end;
	%else %if %length(&fillcolors)=0 %then %do;
		%put ::: NOTE: fillcolors were not set by user, so the option PATTERN was used ;
		%let fillpatterns=X1;
	%end;
	%if %length(&outplot_path)=0 %then %let outplot_path=&CWD;;

	data _null_;
	call symputx("outplot",coalesceC("&outplot","violin_&sysuserid"));
	call symputx("densLAB",coalesceC("&densLAB","&densVAR"));
	call symputx("fillcolors",upcase(coalesceC("&fillcolors","NONE")));
	call symputx("fillpatterns",upcase(coalesceC("&fillpatterns","NONE")));
	if &vertical then do;
		call symputx("orient",coalesceC("&orient","portrait"));
		call symputx("gwidth",coalesceC("&gwidth","8")*1);
		call symputx("gheight",coalesceC("&gheight","12")*1);
		call symputx("VALax","Y");
		call symputx("LABax","X");
	end;
	else do;
		call symputx("orient",coalesceC("&orient","landscape"));
		call symputx("gwidth",coalesceC("&gwidth","12")*1);
		call symputx("gheight",coalesceC("&gheight","8")*1);
		call symputx("VALax","X");
		call symputx("LABax","Y");
	end;
	run;

	%if %length(&labaxis_offset) %then %do;
		%let labaxis_offset=%cmpres(&labaxis_offset);
		%if %sysfunc(countW(&labaxis_offset,' '))=2 %then %do;
			%let laboffset_min=%scan(&labaxis_offset,1,' ');
			%let laboffset_max=%scan(&labaxis_offset,2,' ');
		%end;
		%else %do;
			%let laboffset_min=&labaxis_offset;
			%let laboffset_max=&labaxis_offset;
		%end;
	%end;

	%if %length(&grpfmts) %then %do;
		data _null_;
		call symputx("grpfmts",compbl(tranwrd("&grpfmts",".","")));
		run;
	%end;

	%let grpvars=%cmpres(&grpvars);
	%let ngrpvars=%sysfunc(countW(&grpvars,' '));
	%let nfmts=%sysfunc(countW(&grpfmts,' '));
	* if there are any group formats specified, there must be the same number of formats as group vars ;
	%if &nfmts %then %do;
		%if &nfmts^=&ngrpvars %then %do;
			%put ::: ERROR: if specified, the number of group formats must equal the number of group variables ;
			%abort cancel;
		%end;
	%end;

	%let nsubs=1;
	%if &ngrpvars>1 %then %do;
		%let len_error=0;

		data _indata;
		set &indata;
		array G {*} &grpvars;
		%if &nfmts %then %do;
			%do fn=1 %to &nfmts;  * macro loop avoids problem that GRPVARS may be of different types ;
				%let fmt=%scan(&grpfmts,&fn,' ');
				%let grp=%scan(&grpvars,&fn,' ');
				_v_&fn=put(&grp,&fmt..);
			%end;
			%let catvars=_v_:;
		%end;
		%else %do;
			array G {*} &grpvars; * note this will fail if GRPVARS are not all of the same type ;
			%let catvars=%str(G[*]);
		%end;
		_grpvar=catx('__',of &catvars);
		if length(_grpvar)>25 then do;
			call symputx("len_error",1);
			call symputx("len_error_val",_grpvar);
		end;
		run;

		%if &len_error %then %do;
			%put ::: ERROR: the concatenated group variable &len_error_val is too long - shorten to <=25 char in length incl separators! ;
			%abort cancel;
		%end;

		proc sql noprint;
		select distinct substr(_grpvar,index(_grpvar,'__')+2) into :subs separated by ' ' from _indata;
		select distinct substr(_grpvar,1,index(_grpvar,'__')-1) into :prims separated by ' ' from _indata;
		quit;

		%let nsubs=%sysfunc(countW(&subs,' '));
		data _null_;
		call symputx("qtsubs",prxchange('s/""/"/i',-1,quote(prxchange('s/\s/","/i',-1,"&subs"))));
		run;

		data subfmt;
		length fmtname $8 type $1 start $40 label $40;
		retain fmtname 'fsub2lab' type 'C';
		array S {&nsubs} $40 _temporary_ (&qtsubs);
		do i=1 to dim(S);
			start=S[i];
			label=compbl(tranwrd(tranwrd(prxchange('s/n[0-9]+n//i',-1,S[i]),'_d_','-'),'_',' '));
			output;
		end;
		run;

		proc format cntlin=subfmt; run;

		%let nprims=%sysfunc(countW(&prims,' '));
		%let grpvar=_grpvar;
		%let indata=_indata;
	%end;
	%else %let grpvar=&grpvars;;

	proc means data=&indata noprint;
	class &grpvar;
	var &densvar;
	output out=MM (where=(_type_=1)) mean= median= p25= p75= p5= p95= %if %length(&dens_keepflag)=0 %then %do; min= max= %end; / autoname;
	run;

	proc sort data=MM; by &grpvar; run;

	proc univariate data=&indata noprint;
	class &grpvar;
	var &densvar;
	output out=MMci pctlpre=p pctlpts=2.5 97.5;
	run;

	%if &quantbands=1 and &useCI95 %then %do;
		proc sort data=MMci; by &grpvar; run;

		data MM;
		merge
			MM
			MMci
			;
		by &grpvar;
		&densvar._p5=p2_5;
		&densvar._p95=p97_5;
		run;
	%end;

	%if %length(&dens_keepflag) %then %do;
		proc means data=&indata noprint;
		WHERE &dens_keepflag=1;
		class &grpvar;
		var &densvar;
		output out=MM2 (where=(_type_=1)) min= max= / autoname;
		run;

		proc sort data=MM2; by &grpvar; run;

		data MM;
		merge
			MM
			MM2 (keep=&grpvar &densvar._min &densvar._max)
			;
		by &grpvar;
		run;
	%end;

	%if &denstype=KDE %then %do;
		* the SGPLOT statement below and the two data steps that follow could/should
		probably be changed so that the kernel density estimates are being generated
		by PROC KDE instead of by the SGPLOT DENSITY statement - this would obviate
		the need for the renaming of the strange SGPLOT-generated variables ;
		proc sgplot data=&indata (keep=&densvar &grpvar &dens_keepflag) noautolegend;
		%if %length(&dens_keepflag) %then %do;
			WHERE &dens_keepflag=1;
			%put ::: Plot is only showing obs where &dens_keepflag=1 ;
		%end;
		ods output sgplot=sg;  * <-- this output data contains the KDE generated by the DENSITY statement below ;
		density &densvar / group=&grpvar type=kernel;
		run;
		
		proc contents data=sg noprint out=names (keep=name type); run;
	%end;
	%else %do;
		proc sgplot data=&indata (keep=&densvar &grpvar &dens_keepflag) noautolegend;
		%if %length(&dens_keepflag) %then %do;
			WHERE &dens_keepflag=1;
			%put ::: Plot is only showing obs where &dens_keepflag=1 ;
		%end;
		ods output sgplot=sg;
		histogram &densvar / group=&grpvar binwidth=1;
		run;

		proc contents data=sg noprint out=names (keep=name type); run;
	%end;
	
	data _null_;
	set names (obs=3) end=last;
	length rnm $500;
	retain rnm;
	length new $2;
	new=prxchange('s/.*(GP|X|Y)$/$1/',1,compress(name));	
	if new='GP' then call symputx("grouptype",type);
	rnm=catx(' ',rnm,compress(quote(name) || 'n=' || new));
	if last then call symputx("rnm",rnm);
	run;

	options validvarname=ANY;
	data sg;
	length rownum 8;
	set sg (rename=(&rnm));
	attrib _all_ label=' ';
	rownum=_N_;
	IF not missing(GP);
	run;
	options validvarname=v7;
	
	proc sql noprint;
	select count(distinct GP) into :ngrps trimmed from sg;
	select max(nobs) into :mxobs trimmed from (select GP, count(*) as nobs from sg group by GP);
	select max(Y) into :mxW trimmed from sg;
	create table centers as select distinct GP from sg order by GP;
	quit;
	
	proc sort data=sg; by GP rownum; run;

	proc sql undo_policy=NONE;
	create table sg as
	select a.*, b.minX, b.maxX
	from 
		sg A
		inner join
		(select &grpvar, min(&densvar) as minX, max(&densvar) as maxX
		from &indata %if %length(&dens_keepflag) %then WHERE &dens_keepflag=1; group by &grpvar) B
		on a.GP=b.&grpvar
	order by a.GP, a.rownum;

	create table global_minmax as
	select min(&densvar) as global_minX, max(&densvar) as global_maxX
	from &indata %if %length(&dens_keepflag) %then WHERE &dens_keepflag=1;;
	quit;

	* set Y value to zero and X=. wherever X value extends beyond min or max for a given group (GP) ;
	data 
		sg (drop=trunc_: global_: rename=(rnum=rownum))
		truncXY (keep=GP trunc_: minX maxX global_minX global_maxX)
		;
	if _N_=1 then set global_minmax;
	set sg;
	by GP;
	array Yvals {&mxobs} _temporary_;
	array Xvals {&mxobs} _temporary_;
	retain obsnum has_row1 trunc_minX trunc_maxX trunc_YminX trunc_YmaxX;
	if first.GP then do;
		obsnum=0;
		has_row1=1;
		call missing(of Yvals[*], of Xvals[*], of trunc_:);
	end;
	obsnum+1;
	Yvals[obsnum]=Y;
	Xvals[obsnum]=X;
	if last.GP then do;
		do rownum=1 to obsnum;
			rnum=rownum;
			if NOT (minX<=Xvals[rownum]<=maxX) then do;
				if rownum=1 then has_row1=0; * this row will not be output ;
				Yvals[rownum]=0;
				Xvals[rownum]=.;  * need to reset rownums to start at 1 for first obs ;
			end;
			Y=Yvals[rownum];
			X=Xvals[rownum];
			if X>. then do;
				if trunc_minX=. then do; * this will only be true once for a given GP ;
					trunc_minX=X;
					trunc_YminX=Y;  * value of Y at the X,Y point that has the minimum value of X ;
				end;
				* next 2 keep getting reset until the last allowable value of X (which is always ascending) ;
				trunc_maxX=X;
				trunc_YmaxX=Y;  * value of Y at the X,Y point that has the max value of X ;
				if has_row1=0 then do;
					rnum=1;
					has_row1=1;
					* note: this will cause gaps in row numbers, but this does not really matter - it only matters that the first
					output row has a row number of 1 ;
				end;
				output sg;
			end;
		end;
		output truncXY; * one record per group ;
	end;
	keep GP rnum Y X minX maxX trunc_: global_:;
	run;

	* the truncation step above removed values of X that were outside the bounds of 
	the DENSVAR range.  However, because of the way the points of the KDE are 
	generated, this truncation will result in a situation where the X values no
	longer extend far enough on either side in cases where the min/max for GP(i) 
	do not extend to the global min/max for the input data.  In the next step, 
	interpolate an 'S' (sigmoid) curve that covers the gap (if any) between the 
	last X,Y point (Pt(0) to a point along the line representing the min/max value
	of DENSVAR for GP(i) that is halfway between the Y value at Pt(0) and zero (Pt(1)). ;

	/*
		* Pt(0)
		|	
		\_
		  \
		  |	
	__________x__* Pt(1)    

				*/

	data truncXY;
	set truncXY;
	by GP;
	length type $3 pnt $2 X Y 8;
	* p0=origin, p1=destination ;
	type='max';
	pnt='p0'; X=trunc_maxX; Y=trunc_YmaxX; output;
	pnt='p1'; X=maxX; Y=trunc_YmaxX/2; output;
	
	type='min';
	pnt='p0'; X=trunc_minX; Y=trunc_YminX; output;
	pnt='p1'; X=minX; Y=trunc_YminX/2; output;
	keep GP pnt type X Y global_:;
	run;

	proc sort data=truncXY; by GP type pnt; run;

	data gaps;
	set truncXY;
	by GP type;
	length Xgap 8 curve 3;
	retain Xgap;
	if first.type then Xgap=X;
	else do;
		Xgap=X-Xgap;
		curve=(abs(Xgap/(global_maxX-global_minX))>0.05);
		output;
	end;
	keep GP type curve;
	run;

	proc sql undo_policy=none;
	create table truncXY as
	select a.*, b.curve
	from
		truncXY A
		inner join
		gaps B
		on a.GP=b.GP and a.type=b.type
	order by a.GP, a.type, a.pnt;
	quit;

	%let steps=10;

	data newrows (keep=GP _x _y rename=(_x=X _y=Y));
	set truncXY (drop=global_:);
	by GP type pnt;
	array xy {2,2} _temporary_;
	retain r addpoints rowid;
	if first.type then do;
		call missing(of xy[*]);
		r=0;
		addpoints=0;
	end;
	r+1;
	xy[r,1]=X;
	xy[r,2]=Y;
	if last.type then do;
		if xy[1,1]^=xy[2,1] then do;
			rowid=0;
			addpoints=1;
			array sigx {%eval(&steps+1)} _temporary_;
			array sigy {%eval(&steps+1)} _temporary_;
			diffx=xy[2,1]-xy[1,1];
			diffy=xy[2,2]-xy[1,2];
			do s=1 to dim(sigx);
				sigx[s]=(-&steps/2)+(s-1);
				sigy[s]=1/(1+exp(-sigx[s]));
			end;
			do s=1 to (dim(sigx)-1);
				sigx[s]=xy[1,1]+(s-1)*(diffx/&steps);
			end;
			sigx[dim(sigx)]=xy[2,1];
			do s=1 to (dim(sigy)-1);
				sigy[s]=xy[1,2]+sigy[s]*diffy;
			end;
			sigy[dim(sigy)]=xy[2,2];
			do s=1 to (dim(sigx)-1);
				rowid+1;
				_x=sigx[s];
				_y=sigy[s];
				if curve then output newrows;
			end;
			rowid+1;
			_x=xy[dim(xy,1),1];
			_y=xy[dim(xy,1),2];
			output;
		end;
	end;
	run;

	data sg;
	set
		newrows
		sg (drop=rownum)
		;
	run;

	proc sort data=sg; by GP X; run;

	data sg;
	set sg;
	by GP;
	retain rownum;
	if first.GP then rownum=0;
	rownum+1;
	run;

	proc sort data=sg; by GP rownum; run;	

	data 
		centers (keep=cntr:)
		val2lab (keep=fmtname type start GP rename=(GP=label))
		;
	set centers end=last;
	length cntr1-cntr&ngrps 8 fmtname $8 type $1 start 8;
	array C {*} cntr:;
	retain C start %sysevalf(&mxW/2*-1) fmtname 'fvallab' type 'N';
	start+&mxW;
	output val2lab;
	C[_N_]=start;
	if last then do;
		output centers;
		call symputx("centers",catx(' ', of C[*]));
	end;
	run;

	%if &nsubs>1 %then %do;
		%if %sysfunc(mod(&nsubs,2)) %then %do;
			%let mp=%sysevalf((&nsubs+1)/2);
			data val2lab (drop=mp nlabs);
			set val2lab;
			mp=(&nsubs+1)/2;
			retain nlabs 0;
			if _N_=mp+(nlabs*&nsubs) then do;
				label=substr(label,1,index(label,'__')-1);
				output;
				nlabs+1;
			end;
			run;
		%end;
		%else %do;
			data val2lab (drop=mp nlabs lagstart);
			set val2lab;
			mp=(&nsubs+1)/2;
			retain nlabs 0;
			lagstart=lag(start);
			if _N_=ceil(mp+(nlabs*&nsubs)) then do;
				start=(start+lagstart)/2;
				label=substr(label,1,index(label,'__')-1);
				output;
				nlabs+1;
			end;
			run;
		%end;

		data _null_;
		set val2lab (keep=start) end=last;
		array C {&nprims} _temporary_;
		C[_N_]=start;
		if last then call symputx("centers",catx(' ',of C[*]));
		run;
	%end;

	proc format cntlin=val2lab; run;

	options varlenchk=nowarn;
	data sg; 
	set 
		MM (rename=(&grpvar=GP)) 
		sg (in=B)
		;
	length is_dens 3 r r2 6;
	array p5 {&ngrps} _temporary_;
	array p25 {&ngrps} _temporary_;
	array p75 {&ngrps} _temporary_;
	array p95 {&ngrps} _temporary_;
	retain r 0 r2 0;
	is_dens=B;
	if not B then do;
		r+1;
		p5[r]=&densvar._p5;
		p25[r]=&densvar._p25;
		p75[r]=&densvar._p75;
		p95[r]=&densvar._p95;
	end;
	else do;
		r2+(rownum=1);
		if p25[r2]<=X<=p75[r2] then X2575=X;
		else if p5[r2]<=X<=p95[r2] then X0595=X;
	end;
	run;
	options varlenchk=warn;

	proc sort data=sg; by is_dens GP; run;

	proc sql noprint;
	select min(X) into :minX trimmed from sg;
	select max(X) into :maxX trimmed from sg;
	quit;

	data _null_;
	call symputx("zero",&minX-(&maxX-&minX)*0.05);
	run;

	%if %length(&densRNG) %then %do;
		%let densRNG=%cmpres(&densRNG);
		%let rngmin=%scan(&densRNG,1,%str( ));
		%let zero=%sysfunc(max(&zero,&rngmin));
	%end;

	data sg (rename=(_freq_=count));
	if _N_=1 then set centers;
	set sg;
	by is_dens GP;
	length gnum 6 lbtemp_med hbtemp_med lbtemp_min hbtemp_min lbtemp_max hbtemp_max 
		lowBound highBound lowBound_med highBound_med lowBound_min highBound_min
		lowBound_max highBound_max 8;
	array C {*} cntr:;
	array med {&ngrps} _temporary_;
	array min {&ngrps} _temporary_;
	array max {&ngrps} _temporary_;
	array found_med {&ngrps} _temporary_ (&ngrps*0);
	array found_min {&ngrps} _temporary_ (&ngrps*0);
	array found_max {&ngrps} _temporary_ (&ngrps*0);
	retain gnum lbtemp: hbtemp: lowBound_: highBound_:;
	if first.is_dens then gnum=0;
	gnum+first.GP;
	if is_dens=0 then do;
		med[gnum]=&densvar._median;
		min[gnum]=&densvar._min;
		max[gnum]=&densvar._max;
		statcntr=C[gnum];
		medtxt='+';
		p25txt='-';
		p75txt='-';
		meantxt='X';
		zero=&zero; * not really zero - just a location on the value axis that is 5% below the minimum value of X ;
	end;
	else do;
		if first.GP then call missing(of lbtemp_med, hbtemp_med, lbtemp_min, hbtemp_min, lbtemp_max, hbtemp_max, 
			lowBound_med, highBound_med, lowBound_min, highBound_min, lowBound_max, highBound_max);
		lowBound=C[gnum]-y/2;
		highBound=C[gnum]+y/2;
		if found_med[gnum]=0 then do;
			* medians ;
			if med[gnum]=X then do;
				found_med[gnum]=1;
				lowBound_med=lowBound;
				highBound_med=highBound;
				&densvar._median=med[gnum];
			end;
			else if med[gnum]<X then do;
				found_med[gnum]=1;
				lowBound_med=(lowBound+lbtemp_med)/2;
				highBound_med=(highBound+hbtemp_med)/2;
				&densvar._median=med[gnum];
			end;
			lbtemp_med=lowBound;
			hbtemp_med=highBound;
		end;
		if found_min[gnum]=0 then do;
			* minimums ;
			if min[gnum]=X then do;
				found_min[gnum]=1;
				lowBound_min=lowBound;
				highBound_min=highBound;
				&densvar._min=min[gnum];
			end;
			else if min[gnum]<X then do;
				found_min[gnum]=1;
				lowBound_min=(lowBound+coalesce(lbtemp_min,lowBound))/2;
				highBound_min=(highBound+coalesce(hbtemp_min,highBound))/2;
				&densvar._min=min[gnum];
			end;
			lbtemp_min=lowBound;
			hbtemp_min=highBound;
		end;
		if found_max[gnum]=0 then do;
			* maximums ;
			if max[gnum]=X then do;
				found_max[gnum]=1;
				lowBound_max=lowBound;
				highBound_max=highBound;
				&densvar._max=max[gnum];
			end;
			else if max[gnum]<X then do;
				found_max[gnum]=1;
				lowBound_max=(lowBound+lbtemp_max)/2;
				highBound_max=(highBound+hbtemp_max)/2;
				&densvar._max=max[gnum];
			end;
			else if last.GP then do;
				found_max[gnum]=1;
				lowBound_max=lowBound;
				highBound_max=highBound;
				&densvar._max=max[gnum];
			end;
			lbtemp_max=lowBound;
			hbtemp_max=highBound;
		end;
	end;
	run;

	%let nlab=0;
	%if &nsubs>1 %then %do;
		data sg;
		if _N_=1 then set global_minmax;
		set sg;
		array S {&nsubs} $40 _temporary_ (&qtsubs);
		array C {*} cntr:;
		length GP_1-GP_&nsubs $40 X_1-X_&nsubs lowBound_1-lowBound_&nsubs highBound_1-highBound_&nsubs 
			X0595_1-X0595_&nsubs X2575_1-X2575_&nsubs 8;
		array G {*} GP_:;
		array _x {*} X_:;
		array lb {*} lowBound_1-lowBound_&nsubs;
		array hb {*} highBound_1-highBound_&nsubs;
		array x05 {*} X0595_:;
		array x25 {*} X2575_:;
		%if %length(&legend_loc_valaxis) and %length(&legend_loc_labaxis) %then %do;
			if _N_=1 then do;	
				length 
					bsize 3 vertsep horizsep 
					legtxt_labax_1-legtxt_labax_&nsubs legtxt_valax_1-legtxt_valax_&nsubs
					legbub_labax_1-legbub_labax_&nsubs legbub_valax_1-legbub_valax_&nsubs 8
					legtxt_1-legtxt_&nsubs $40;
				bsize=20;
				%if &vertical %then %do;
					vertsep=(global_maxX-global_minX)*0.03;
					horizsep=(C[dim(C)]-C[1])*0.02;
				%end;
				%else %do;
					vertsep=(C[dim(C)]-C[1])*0.03;
					horizsep=(global_maxX-global_minX)*0.02;
				%end;
				array t_lab {*} legtxt_lab:;
				array t_val {*} legtxt_val:;
				array b_lab {*} legbub_lab:;
				array b_val {*} legbub_val:;
				array text {*} legtxt_1-legtxt_&nsubs;
				* find the actual location on the label axis referenced by the value specified in <legend_loc_labaxis>,
				which is relative to the violin count - e.g., <legend_loc_labaxis>=7.3 means the location that is
				30% of the way betweeen the 7th violin and the 8th. -- to find the actual location for that example on 
				the label axis, find the value in array C that is 30% of the way between C[7] and C[8].  Note that 
				if <legend_loc_labaxis> is an integer, then the actual location calculated below is simply C[<legend_loc_labaxis>] ;
				floorC=C[floor(&legend_loc_labaxis)];
				ceilC=C[ceil(&legend_loc_labaxis)];
				legloc_actual=floorC+(ceilC-floorC)*(&legend_loc_labaxis-floor(&legend_loc_labaxis));
				do i=1 to &nsubs;
					%if &vertical=1 %then %do;    * lab axis is X when vertical=1 and Y when vertical=0 ;
						t_lab[i]=legloc_actual-horizsep;
						t_val[i]=&legend_loc_valaxis-(i-1)*vertsep;
						b_lab[i]=legloc_actual;
						b_val[i]=&legend_loc_valaxis-(i-1)*vertsep;
					%end;
					%else %do;
						t_lab[i]=legloc_actual-(i-1)*vertsep;
						t_val[i]=&legend_loc_valaxis-horizsep;
						b_lab[i]=legloc_actual-(i-1)*vertsep;
						b_val[i]=&legend_loc_valaxis;
					%end;
					text[i]=put(S[i],$fsub2lab.);
				end;
				if dim(C)>1 then do;
					Nlabax=C[1]-(C[2]-C[1])/2;
					Ntxt='N:';
					call symputx("nlab",1);
				end;
			end;
		%end;
		do i=1 to dim(S);
			if S[i]=substr(GP,index(GP,'__')+2) then do;
				G[i]=GP;
				_x[i]=X;
				lb[i]=lowBound;
				hb[i]=highBound;
				x05[i]=X0595;
				x25[i]=X2575;
			end;
		end;
		drop i;
		run;
	%end;
	
	%let anycomps=0;

	%if %length(&comparisons) %then %do;
		proc sort data=sg; by is_dens GP rownum; run;

		%let comparisons=%lowcase(%cmpres(&comparisons));
		%if &comparisons=all %then %do;
			%if ngrps=1 %then %do;
				%let anycomps=0;
				%goto mkplot;
			%end;

			%let comparisons=;
			%do cn=2 %to &ngrps;
				%let comparisons=&comparisons &cn.v1; * i.e., grp 2 vs grp 1, grp 3 vs grp 1, ... grp<ngrps> vs grp 1 ;
			%end;
		%end;
		%let anycomps=1;

		%let ncomp=%sysfunc(countW(&comparisons,' '));

		%include '/data/prod/common/WRJ_macros/js.bootstrap.sas';

		proc sql undo_policy=NONE;
		create table mx as
		select GP, max(maxX) as maxX
		from sg
		where is_dens=1
		group by GP;

		create table sg as
		select a.*, b.maxX
		from
			(select * from sg (drop=maxX)) A
			inner join
			mx B
			on a.GP=b.GP
		order by a.is_dens, a.GP, a.rownum;
		quit;

		data lookups;
		set sg (keep=is_dens GP cntr: maxX WHERE=(is_dens=1));
		by is_dens GP;
		array C {*} cntr:;
		length grpnum 4 center height sepBr sep 8;
		sepBr=(&maxX-&minX)*0.045;
		retain grpnum 0 sep 0;
		if first.GP then do;
			grpnum+1;
			center=C[grpnum];
			height=maxX+sep;
			output;
			sep+sepBr;
		end;
		keep GP grpnum center height;
		run;

		%do cnum=1 %to &ncomp;
			
			%let comp=%scan(&comparisons,&cnum,' ');

			* e.g., if comp=3v1 (grp 3 vs. grp 1), then comp0 (unexposed) is grp 1 and comp1 (exposed) is grp 3 ;
			%let comp0=%scan(&comp,2,v);
			%let comp1=%scan(&comp,1,v);

			data comps;
			set lookups (WHERE=(grpnum in (&comp0, &comp1))) end=last;
			length comp $5 comp0ctr comp1ctr comp0max comp1max compmax 8;
			retain comp "&comp" comp0: comp1:;
			if grpnum=&comp0 then do;
				call symputx("comp0name",GP);
				comp0ctr=center;
				comp0max=height;
			end;
			else do;
				call symputx("comp1name",GP);
				comp1ctr=center;
				comp1max=height;
			end;
			if last then do;
				compmax=max(comp0max, comp1max)+(&maxX-&minX)*0.02;
				output;
			end;
			keep comp:;
			run;

			%bootmdiff (
				boottype=&comparison_type,	/* options: MEAN or MEDIAN */
				indata=&indata, 	/* input SAS dataset */
				ptid=,		/* variable representing the patient identifier -- LEAVE BLANK IF INDATA IS NOT ONE ROW PER PATIENT! */
				invar=&densvar, 	/* numeric variable for which to estimate median/mean and CI */
				invar_lab=,	/* optional - if provided, will be used in plot title to represent <INVAR> */
				byvar=&grpvar, 	/* categorical variable to differentiate one set of <INVAR> values from another */
				byvar_levs=&comp0name &comp1name,	/* space-separated list of <byvar> levels, e.g., 0 1 2 3, for pair-wise comparisons (using the first 
							as the referent group each time, e.g., 0vs1 0vs2 0vs3) */
				byvar_lab=,	/* optional - if provided, will be used in plot title to represent <BYVAR> */
				reps=1000, 	/* number of repetitions for bootstrap -- min: 1000 */
				mkplot=0,	/* make a histogram - only available if byvar_levs contains exactly 2 values */
				textfmt=&comparison_fmt,	/* format to apply to <INVAR>, e.g., dollar8., 3.1, percent8.1 - defaults to BEST. */
				CI_sep=%str(, ), /* separator for lower and upper bounds of confidence interval */
				outsuff=&comp,	/* optional - if provided, suffix that will be added to output dataset name called WORK.all_m, i.e., 
							WORK.all_m&outsuff - an underscore will be added automatically */
				shortlog=1	/* 0: maintain log for each iteration of loop, 1: only show first iteration in log */
				); /* output: result in .lst printout, WORK.all_m, WORK.all_CI, plots (optional) */
			

			/*
			 make X/Y coordinates to draw brackets for this comparison, e.g., 

					3.5 (2.5-5.0)
				______|_____	
				|          |
				|			
						*/

			data comps;
			if _N_=1 then set all_CI_&comp (keep=mdiff_CI);
			set comps;
			length segment $15 labBr0 labBr1 valBr0 valBr1 diffText_labAx diffText_valAx 8;
			segment="plot &comp0 arm"; labBr0=comp0ctr; labBr1=comp0ctr; valBr0=comp0max; valBr1=compmax; output;
			segment="plot &comp1 arm"; labBr0=comp1ctr; labBr1=comp1ctr; valBr0=comp1max; valBr1=compmax; output;
			segment="span &comp0-&comp1"; labBr0=comp0ctr; labBr1=comp1ctr; valBr0=compmax; valBr1=compmax; output;
			segment="cntr tick &comp0-&comp1"; labBr0=(comp0ctr+comp1ctr)*0.55; labBr1=labBr0; valBr0=compmax; valBr1=compmax+(&maxX-&minX)*0.01; output;
			segment="diff &comp1 vs &comp0"; diffText_labAx=labBr0; diffText_valAx=valBr1; output;
			keep comp segment labBr: valBr: diffText: mdiff_CI;
			run;

			proc append data=comps base=all_comps; run;

		%end;

		title "comp draw instructions";
		proc print data=all_comps width=min;  run;
		title;

		data sg %if &comparison_descrip %then %do; (rename=(_d=mdiff_CI)) %end;;
		set 
			sg
			all_comps
			;
		%if &comparison_descrip %then %do;
			length _d $40;
			%if &comparison_type=MEDIAN %then %do;
				_dpre="med diff ";
			%end;
			%else %do;
				_dpre="mean diff ";
			%end;
			comp1=scan(comp,1,'v');
			comp0=scan(comp,2,'v');
			if diffText_labax>. then _d=compbl(_dpre || compress("(" || comp1) || " vs " || compress(comp0 || "):") || " " || mdiff_CI);
			drop mdiff_CI comp1 comp0;
		%end; 
		run;
		
		%if &comparison_calc_only %then %do;
			%let anycomps=0;
			%put ::: You selected the option to exclude comparisons from the plot - results are in .lst file ;
		%end;

	%end;

	%mkplot:	

	ods listing close;
	ods graphics on / 
		imagefmt=tiff
		imagename="&outplot"
		noborder
		width=&gwidth.in height=&gheight.in 
		imagemap=on
		;

	ods listing gpath="&outplot_path" image_dpi=100;

	goptions gsfmode=replace;
	options orientation=&orient;

		proc sgplot data=sg noautolegend;
		%if %length(&outplot_title) %then title "&outplot_title";;
		&LABax.axis display=(nolabel) values=(&centers) valuesformat=fvallab. %if &vertical %then fitpolicy=ROTATE; 
			offsetmin=&laboffset_min offsetmax=&laboffset_max %if &vertical=0 %then reverse;;
		&VALax.axis label="&densLAB" grid gridattrs=(color=gray pattern=dot thickness=1) 
			%if %length(&densRNG) %then %do; values=(&densRNG) %end; %if &show_counts %then %do; offsetmin=0.07 %end;
			%if &anycomps %then %do; offsetmax=%sysfunc(min(0.4,%sysevalf(0.07*&ncomp+(&ncomp=1)*0.05))) %end;;
		%do s=1 %to &nsubs;
			%let ext=;
			%if &nsubs>1 %then %let ext=_&s;;
			%let color=%sysfunc(coalesceC(%scan(&fillcolors,&s,' '),NONE));
			%let pattern=%sysfunc(coalesceC(%scan(&fillpatterns,&s,' '),NONE));
			band &valAX=X&ext lower=lowBound&ext upper=highBound&ext / group=GP&ext %if &outline %then outline lineattrs=(pattern=solid color=black);
				%if &color=NONE %then %do;
					NOFILL
				%end;
				%else %do;
					FILL fillattrs=(color=&color)
				%end;
				%if &pattern^=NONE %then %do;
					fillpattern
					fillpatternattrs=(pattern=&pattern color=black)
				%end;
				transparency=0.4
				;
			%if &quantbands %then %do;
				band &valAX=X0595&ext lower=lowBound&ext upper=highBound&ext / group=GP 
					fillattrs=(color=cx6BAED6) 
					%if &pattern^=NONE %then %do;
						fillpattern
						fillpatternattrs=(pattern=&pattern color=black)
					%end;
					transparency=0.4;
				band &valAX=X2575&ext lower=lowBound&ext upper=highBound&ext / group=GP 
					fillattrs=(color=cx3182BD) 
					%if &pattern^=NONE %then %do;
						fillpattern
						fillpatternattrs=(pattern=&pattern color=black)
					%end;
					transparency=0;
			%end;
		%end;
		%if &nsubs>1 and %length(&legend_loc_valaxis) and %length(&legend_loc_labaxis) %then %do;
			%do s=1 %to &nsubs;
				%let color=%sysfunc(coalesceC(%scan(&fillcolors,&s,' '),NONE));
				%let pattern=%sysfunc(coalesceC(%scan(&fillpatterns,&s,' '),NONE));
				bubble &VALax=legbub_valax_&s &LABax=legbub_labax_&s size=bsize / 
					%if %length(&color) and &color^=NONE %then fill fillattrs=(color=&color);
					%if %length(&pattern) and &pattern^=NONE %then fillpattern fillpatternattrs=(pattern=&pattern color=darkgrey);
					transparency=0.4 outline
					;
				text &VALax=legtxt_valax_&s &LABax=legtxt_labax_&s text=legtxt_&s / position=LEFT textattrs=(size=12pt color=black);
			%end;
		%end;				
		highlow &VALax=&densvar._median low=lowBound_med high=highBound_med  / group=GP type=line 
			lineattrs=(thickness=2 color=red pattern=solid) lowcap=none highcap=none;
		highlow &VALax=&densvar._min low=lowBound_min high=highBound_min  / group=GP type=line 
			lineattrs=(thickness=1 color=black pattern=solid) lowcap=none highcap=none;
		highlow &VALax=&densvar._max low=lowBound_max high=highBound_max  / group=GP type=line 
			lineattrs=(thickness=1 color=black pattern=solid) lowcap=none highcap=none;
		text &LABax=statcntr &VALax=&densvar._mean text=meantxt;
		%if &show_counts %then %do;
			%if &nlab %then %do;
				text &LABax=Nlabax &VALax=zero text=Ntxt / position=BOTTOM textattrs=(size=8pt color=black weight=bold);
			%end;
			text &LABax=statcntr &VALax=zero text=count / group=GP position=BOTTOM textattrs=(size=8pt color=black);
		%end;
		%if %length(&valaxis_refline_statement) %then %do;
			&valaxis_refline_statement axis=&VALax;
		%end;
		%if &anycomps %then %do;
			vector &LABax=labBr1 &VALax=valBr1 / group=comp NOARROWHEADS &LABax.origin=labBr0 &VALax.origin=valBr0 
				lineattrs=(thickness=1 pattern=solid color=black);
			text &LABax=diffText_labax &VALax=diffText_valax text=mdiff_CI / group=comp 
				position=TOPRIGHT textattrs=(size=%if &ncomp>1 %then 8pt; %else 10pt; color=black);
		%end;
		%if %length(&footnote) %then footnote height=6pt italic justify=right "&footnote";;
		run;

	ods listing close;
	ods listing;
	
	proc datasets lib=work memtype=data nolist nodetails; 
	SAVE &worktabs; 
	run; quit;

%mend; *violin();

