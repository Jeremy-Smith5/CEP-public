/*
	Collapse overlapping or adjacent stays into contiguous windows.  If input data contains dates, 
	gaps are calculated in days; if datetimes, gaps are calculated in seconds.  Note use of the 
	collapse_adjacent and collapse_adjacent_gap parameters as described below.

	12 Jan 2018
	
	Jeremy Smith

	NOTE: an example call for this macro is included at the bottom.

	================================================================================================== */

%MACRO collapse_stays(
	ipdata=,	/* input dataset: <ptvar>, <adt>, <ddt> (<adt> and <ddt> can be dates or datetimes, but specify using is_datetime parameter) */
	adt=admitdt,	/* name of admit date variable */
	ddt=dischdt,	/* name of discharge date variable */
	is_datetime=1,	/* if date variables provided are datetimes -- set to 1 */
	collapse_adjacent=1,  /* if admitted again on same day (or second, in the case of datetimes) as discharge, collapse */
	collapse_adjacent_gap=0, /* if collapse_adjacent set to 1, use this param to set the maxium gap to span - e.g., 
		if used with dates (rather than datetimes), and _gap is set to 0, then the macro will collapse stays that
		are separated by 0 days (i.e., admitted again same day as previous discharge). If set to, e.g., 2, the macro
		will collapse stays where next admit is at most 2 days after prior discharge.  Note this param is interpreted
		as # of seconds in the case of datetimes */
	rename_vars=0,	/* rename date variables on output file to match those from input */
	ptvar=patientICN	/* name of patient variable */
	); /* output: work.collapsed_stays (<ptvar>, <adt>, <ddt>) */

	%if &is_datetime %then %do;
		%put ::: IMPORTANT: since you are using datetimes as the input for the collapse_stays macro, gaps are calculated in seconds rather than days! ;
	%end;
	
	%let any_dropped_stays=0;

	data 
		collapsed_stays
		dropped_stays
		;
	set &ipdata;
	length LOS 8;
	LOS=&ddt-&adt;
	if LOS=. then do;
		call symputx("any_dropped_stays",1);
		output dropped_stays;
	end;
	else output collapsed_stays;
	run;

	%if &any_dropped_stays %then %do;
		proc sql;
		title "# of stays dropped d/t missing admit or discharge";
		select count(*) from dropped_stays;
		quit;
		title;
	%end;

	proc sort data=collapsed_stays
		NODUPKEY; 
	by 
		&ptvar
		&adt
		DESCENDING LOS
		;
	run;

	data collapsed_stays (keep=&ptvar admit2 disch2);
	set collapsed_stays;
	by 
		&ptvar
		&adt
		DESCENDING LOS
		;
	length admit2 disch2 8;
	retain admit2 disch2;
	format admit2 disch2 %if &is_datetime %then datetime20.; %else date9.;;
	if first.&ptvar then do;
		admit2=&adt;
		disch2=&ddt;
	end;
	* first check whether the current record admit/disch dates completely absorb the one in memory -
		note that for first.patient, this will always be true ;
	if last.&ptvar and ((&adt>=admit2)*(&ddt<=disch2)) then output;
		
	else if NOT ((&adt<disch2)*(&ddt>admit2)) then do;
		* new record does not overlap with the prior one in memory, so output the one in memory ;
		output;
		*... then update the retained admit/disch dates ;
		admit2=&adt;
		disch2=&ddt;
		if last.&ptvar then output;
	end;
	else if (disch2<&ddt) then do;
		disch2=&ddt;
		if last.&ptvar then output;
	end;
	run; 

	%if &collapse_adjacent %then %do;

		* optionally, collapse adjacent stays too:  discharged and then admitted on same day ;
		proc sort data=collapsed_stays; by &ptvar admit2; run;

		data collapsed_stays (keep=&ptvar _admit _disch rename=(_admit=admit2 _disch=disch2));
		set collapsed_stays;
		by &ptvar admit2;
		length _admit _disch 8;
		format _admit _disch date9.;
		retain _admit _disch;
		if first.&ptvar then do;
			_admit=admit2;
			_disch=disch2;
		end;
		if admit2 > _disch + &collapse_adjacent_gap then do;
			output;
			_admit=admit2;
		end;
		_disch=disch2;
		if last.&ptvar then output;
		run;

		* if gap was set to >0, output a note to verify the output ;
		%if &collapse_adjacent_gap %then %do;
			%put ::: IMPORTANT: you set a max allowed gap of more than 0 days/seconds - be sure to test this with the simulated data example! ;
		%end;

	%end;

	%if &rename_vars %then %do;
		
		proc datasets lib=work memtype=data nolist nodetails;
		modify collapsed_stays;
		rename 
			admit2=&adt
			disch2=&ddt
			;
		run; quit;

	%end;

%MEND; *collapse_stays();

/* 
	* simulated data for testing collapse_stays macro ;

	data teststays;
	length fakeID 3 admitdate dischdate 5;
	format admitdate dischdate date9.;
	fakeID=101;
	admitdate='05Feb2021'd;
	dischdate='09Feb2021'd;
	output;
	admitdate='05Feb2021'd;
	dischdate='06Feb2021'd;
	output;
	admitdate='08Feb2021'd;
	dischdate='09Feb2021'd;
	output;
	admitdate='07Feb2021'd;
	dischdate='15Feb2021'd;
	output;
	admitdate='15Feb2021'd;
	dischdate='21Feb2021'd;
	output;
	admitdate='22Feb2021'd;
	dischdate='26Feb2021'd;
	output;
	run;

	%collapse_stays(
		ipdata=teststays,
		adt=admitdate,
		ddt=dischdate,
		is_datetime=0,
		collapse_adjacent=1,
		collapse_adjacent_gap=0,
		rename_vars=1,
		ptvar=fakeID
		);

	proc sql;
	title "original data -- # of obs";
	select count(*) from teststays;
	title "collapsed data -- # of obs";
	select count(*) from collapsed_stays;
	quit;

	title "original";
	proc print data=teststays width=min; run;

	title "collapsed";
	proc print data=collapsed_stays width=min; run; */


