/*
Make a 'turnip' plot (one for each level of <groupvar>) using SAS PROC SGPLOT
J Smith
2017
last update: 2022

============================================================================ */

%macro turnip(
	indata=/*km.v2_refs_by_provider*/, 
	mkrates=0,
	raw_rate=, /* var name for rate (if mkrates=0) */
	rate_fmt=,  /* format (if any) to apply to rate, e.g., percent8.1 */
	ratelab=,  /* label for y-axis of plot */
	ratemult=1,  /* multiplier for rate (if mkrates=1) */
	R=0.1,  /* level of rounding for creating bins */
	statshift=1, /* multiplier for R by which to separate the 'median' label from the value */
	show_change=0, /* 0 - do not show changes (as described below) ... 1 - show changes */
	change_type=,	/* TOP or FLAGGED (if FLAGGED, input dataset must contain a 0/1 variable called 'change_flag') */
	change_top_n=5, /* only applicable if SHOW_CHANGE is set to 1 -- 1-n - add arrow and label connecting points in 
		1 turnip to the adjacent for groups with largest (n) changes or flagged changes (in development) - only
		relevant when individual turnips represent changes over time for the same groups. */
	denvar=,    /* name of variable containing denominator */
	denvarlab=,  /* label for denominator */
	numvar=,    /* name of variable containing numerator */
	groupvar=,  /* e.g., region, specialty -- the variable that differentiates the turnips */
	membervar=,  /* e.g., ACO ID - the variable that corresponds to each bubble */
	memberlabvar=,  /* variable that contains name of the member - if blank, defaults to membervar */
	title=turnips,
	T2=,
	foot1=,
	foot2=,
	bubblemin=3,
	bubblemax=10,
	tipvars=,
	tiplabs=,
	colorvar=,
	color_fmt=,
	colortype=,
	exampleplot=0
	);
	
	%if &R= %then %let R=0.1;;              ** rounding level ; 
	%if &statshift= %then %let statshift=1;;
	%if &show_change= %then %let show_change=0;;
	%if &bubblemin= %then %let bubblemin=3;;
	%if &bubblemax= %then %let bubblemax=%eval(&bubblemin+7);;
	%if &rate_fmt^= %then %do;
		%if %substr(&rate_fmt,%length(&rate_fmt),1)=. %then %do;
			%let rate_fmt=%substr(&rate_fmt,1,%eval(%length(&rate_fmt)-1));
		%end;
		%if %index(&rate_fmt,.)=0 %then %do;
			%let rate_fmt=&rate_fmt..;  * this prevents adding a dot to the end of, e.g., percent8.1 ;
		%end;
	%end;
	%if &color_fmt^= %then %do;
		%if %substr(&color_fmt,%length(&color_fmt),1)=. %then %do;
			%let color_fmt=%substr(&color_fmt,1,%eval(%length(&color_fmt)-1));
		%end;
		%if %index(&color_fmt,.)=0 %then %do;
			%let color_fmt=&color_fmt..;  * this prevents adding a dot to the end of, e.g., percent8.1 ;
		%end;
	%end;

	data forplot0;
	set &INDATA;
	%if &mkrates %then %do;
		WHERE &denvar>=&dencut and &numvar>=&numcut;
		rate=round(&numvar/&denvar*&ratemult,&R);
		raw_rate=&numvar/&denvar*&ratemult;
		%let raw_rate=raw_rate;
	%end;
	%else %do;
		rate=round(&raw_rate,&R);
	%end;
	run;

	proc sql noprint;
	create table bins2 as select distinct &groupvar, rate, 
	count(*) as nmembers_bin  
	from forplot0 group by &groupvar, rate 
	order by &groupvar, rate;

	select max(nmembers_bin) into :maxw from bins2;
	quit;
	
	proc means data=forplot0 noprint;
	class &groupvar;
	var &raw_rate;
	output out=meds median=MED mean=mean n=n min=min max=max;
	run;
	
	proc means data=forplot0 noprint;
	class &groupvar;
	WEIGHT &denvar;
	var &raw_rate;
	output out=wtmeans mean=wt_mean;
	run;

	data meds;
	set meds;
	WHERE missing(&groupvar)=0;
	if mod(MED,&R)=0 then mp=MED+(&R/2); * if using 'RAW' median, this is not relevant ;
	else mp=MED;
	
	MP=MED;
	
	length median $%eval(&maxw+2);
	median=repeat('-',%eval(&maxw+1));
	run;
	
	proc sort data=meds; by &groupvar; run;
	proc sort data=wtmeans (where=(missing(&groupvar)=0)); by &groupvar; run;
	proc sort data=forplot0; by &groupvar; run;
	
	data forplot0;
	merge
		forplot0 (in=A)
		meds (in=B keep=&groupvar med mp median mean n min max)
		wtmeans (in=C keep=&groupvar wt_mean)
		;
	by &groupvar;
	IF A;
	run;
	
	proc sort data=forplot0; by &groupvar; run;
	
	data forplot0 (rename=(med=medval mp=medvalpos));
	set forplot0;
	by &groupvar;
	if not first.&groupvar then do;
		med=.;
		mp=.;
		median='';
		mean=.;
		n=.;
		min=.;
		max=.;
		wt_mean=.;
	end;
	run;		
	
	proc sort data=forplot0; by &groupvar rate; run;
	
	data forplot0;
	merge
		forplot0 (in=A)
		bins2 (in=B)
		;
	by &groupvar rate;
	if A*B; 
	run;
	
	proc sql noprint;
	create table groupnums as
	select distinct &groupvar
	from forplot0
	order by &groupvar;
	quit;
	
	data _null_;
	length groupnum 3;
	set groupnums;
	groupnum=_N_;
	call symputx(compress("group" || _N_),&groupvar);
	call symputx("ngroups",_N_);
	run;
	
	%let maxw=%sysfunc(compress(&maxw));
	data _null_;
	call symputx("XLEN",&ngroups*&maxw+%eval(&ngroups+1)*(&maxw/2));
	run;
	
	data _null_;
	call symputx("left",(&maxw/2)*1-2);
	%let mlt=-1;
	%do g=1 %to &ngroups;
		%let mlt=%eval(&mlt+3);
		call symputx("mid&g",(&maxw/2)*&mlt);
	%end;
	%let mlt=%eval(&mlt+1);
	call symputx("right",(&maxw/2)*&mlt+2);
	run;
	
	data forplot0; 
	set forplot0;
	by &groupvar rate;
	retain memnum;
	if first.rate then memnum=0;
	memnum+1;
	Xpos=(nmembers_bin/2+0.5-memnum)*-1;
	%do g=1 %to &ngroups;
		if &groupvar="&&group&g" then do;
			Xpos=Xpos+&&mid&g;
		end;
	%end;
	run;
	
	proc sql noprint;
	%do g=1 %to &ngroups;
		select count(*) into :n&g from forplot0 where &groupvar="&&group&g";
	%end;
	
	create table maxY as select distinct a.&groupvar, 
	max(a.rate*1.1) as maxY_&groupvar,
	b.maxY_all
	from 
		forplot0 A, 
		(select max(rate*1.1) as maxY_all from forplot0) B
	group by a.&groupvar
	order by a.&groupvar;
	
	create table forplot as select a.*, b.maxY_&groupvar, b.maxY_all
	from forplot0 A, maxY B where a.&groupvar=b.&groupvar
	order by a.&groupvar, a.&membervar;
	quit;

	ods escapechar='~';

	%do g=1 %to &ngroups;
		%let n&g=%sysfunc(compress(&&n&g));
	%end;

	proc format;
	value fgroupnum
		%do g=1 %to &ngroups;
			&&mid&g="&&group&g (N=&&n&g)"
		%end;
		other = ' '
		;
	run;

	data forplot;
	set forplot;
	by &groupvar &membervar;
	if not missing(medval) then do;
		med=compbl(put(medval,8.2) || byte(151) || byte(155));
		statlab="median";
		statY=medvalpos+(&R*&statshift);
		%do g=1 %to &ngroups;
			if &groupvar="&&group&g" then do;
				Xpos_group=&&mid&g;
				Xpos_grouplab=compbl("N: &&n&g Med: " || put(medval,8.2));
				medX=floor(&&mid&g-&maxw/2)-2;
			end;
		%end;
	end;
	run;

	%if &memberlabvar= %then %let memberlabvar=&membervar;;

	%if &show_change %then %do;
		* make an annotate dataset that contains the start and stop x,y cooordinates for 
		arrows connecting groups with the largest + or - changes between timepoints ;
		%if &change_type= %then %let change_type=TOP;;
		%if &change_top_n= %then %let change_top_n=5;;
		%let change_type=%upcase(&change_type);

		data arrows;
		set forplot;
		WHERE not missing(&memberlabvar) and Xpos>. and rate>.;
		run;

		%if &change_type=FLAGGED %then %do;
			data arrows;
			set arrows (WHERE=(change_flag=1));
			call symputx("change_top_n",_N_);
			run;
		%end;
		
		proc sort data=arrows NODUPKEY; by &memberlabvar Xpos; run;

		data arrows;
		set arrows (keep=&memberlabvar &groupvar Xpos rate);
		by &memberlabvar;
		length x1 x2 y1 y2 change 8;
		retain x2 y2;
		if first.&memberlabvar then do;
			x2=Xpos;
			y2=rate;
		end;
		x1=x2;
		y1=y2;
		x2=Xpos;
		y2=rate;
		change=ABS(y2-y1);
		if not first.&memberlabvar then output;
		keep &memberlabvar x1 x2 y1 y2 change;
		run;

		proc sort data=arrows; by descending change; run;

		data arrows (drop=&memberlabvar change);
		set arrows (obs=&change_top_n);
		retain drawspace 'datavalue' function 'arrow' shape 'barbed' direction 'out' linecolor 'lightgray' linethickness 1 textsize 6;
		run;

		/*
		ods listing;
		title "arrows data";
		proc print data=arrows heading=v width=min; run;
		ods listing close;
		endsas; */
	%end;

	%let groupfmt=fgroupnum.;
	
	%if &colortype= %then %let colortype=&colorvar;;
	
	proc sgplot data=forplot noautolegend %if &show_change %then %do; sganno=arrows %end;;
	title "&title";
	%if &foot1^= %then %do;
		footnote height=0.65 italic justify=right "&foot1";
	%end;
	%if &foot2^= %then %do;
		footnote height=0.65 italic justify=right "&foot2";
	%end; 
	format Xpos &groupfmt %if &rate_fmt^= %then %do; rate &rate_fmt %end; %if &color_fmt^= %then %do; &colorvar &color_fmt %end;;
	scatter y=medvalpos x=medX / 
		markerchar=med
		markercharattrs=(size=6 weight=bold color=black)
		tip=(&groupvar n medval mean wt_mean min max)
		tiplabel=("&groupvar" "# members" "Median" "Mean (crude)" "Mean (&denvar-weighted)" "Min" "Max")
		name="lines"
		; 
	scatter y=statY x=medX / 
		markerchar=statlab
		markercharattrs=(size=6 style=italic weight=bold color=black)
		tip=(&groupvar) tiplabel=("&groupvar")
		name="statlabel"
		; 
	BUBBLE y=rate x=Xpos size=&denvar /
		colorresponse=&colorvar name="bubbles"
		colormodel=(lightblue blue purple red)
		bradiusmin=&bubblemin bradiusmax=&bubblemax
		draworder=data
		dataskin=sheen
		tip=(&memberlabvar &denvar  
		&tipvars) 
		tiplabel=("member name" "&denvarlab"  
		&tiplabs)
		;
	gradlegend "bubbles" / title="&colortype";
	xaxis values=(&left %do g=1 %to &ngroups; &&mid&g %end; &right) 
		display=(nolabel noticks);
	yaxis grid label="&ratelab";
	keylegend / exclude=("maxY_all" "medvalpos" "statY");
	run; 

%mend; *turnip();
