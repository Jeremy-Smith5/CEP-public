/*
	Using the daily, county-level Covid case data from the NY Times Github site and a Census file
	with overall county populations, create a moving average (of dynamic length) of Covid-19 case
	rates by day.  Optionally, join this file to a provided patient- or patient-event-level file 
	with county and date so that, as of given date (or dates) relevant to the patient, we can 
	adjust for current community Covid exposure. 

	Note that the county-level data is only consistently available starting around March 2020, so 
	its best to not set RTSTART to any date earlier than that (except for specific counties).  
	RTEND may be left blank - in which case, the macro will use all available data (generally, we
	have been updating this from the Github site monthly (as of April 2022).  However, be aware 
	that county reporting has become more sporadic as has the overall popularity of testing and
	the reporting of that testing (esp. since at-home tests became more common).  Further, federal
	funding that allowed for free Covid testing ran out in March 2022.

	LBSTART / LBEND: these are the number of days to look back, relative to the date in question, 
	for the start and end, respectively, of the moving average.  For example, if LBSTART is set to 
	16 and LBEND to 2, then the moving average calculated for, e.g., March 20th 2021 will be based
	on Covid cases occurring (within that county) between March 4th and March 18th.

	In setting LBSTART/LBEND, remember to take into account incubation period for Covid, depending
	on your study question.

	J Smith
	March 2022

	OUTPUTS:
		- WORK.county_covid_rates -- this is a county-day level file showing the calculated Covid
		case moving average per 10k county population.
		- SAS format frtq:  this allows easy binning of the rates into calculated quintiles
		- <JOINDATA> (if joindata were provided in the macro call) -- this OVERWRITES the input
		file with an identical file that has county Covid moving average rate for county/day.

	================================================================================================ */
 
%MACRO county_covid_rates(
	rtstart=01Mar2020,
	rtend=,
	LBstart=16,
	LBend=2,
	joindata=,	/* optional pt or event-level file: if provided, rates will be joined to input file on FIPS and 
			<joindata_datevar> */
	joindata_datevar=,	/* if <joindata> is provided, this is the date variable in that file to be joined on */
	covidfile=/data/prod/common/WRJ_macros/zip_county_files/narrow_us_counties_cov19_from_nyt_github.csv,
	popfile=/data/prod/common/WRJ_macros/zip_county_files/co_est2019_alldata.csv
	);

	%let blocklen=%eval(&LBstart-&LBend);
	%let blocklag=%eval(&LBend+1);

	%if &rtstart= %then %do;
		%let rtstart=01Mar2020;
	%end;

	* get US cases (this was downloaded from the NY Times github site 23 Nov 2020) - last updated 31 Mar 2021 ;

	proc import out=UScases dbms=csv replace
		datafile="&covidfile";
	run;

	data UScases (rename=(_fips=fips));
	length _fips $5 state $2;
	set UScases (drop=county state);
	_fips=put(fips,z5.);
	state=fipstate(substr(_fips,1,2)*1);
	drop fips;
	if missing(state) then delete;  * about 1 pct of records have a missing FIPS ;
	run;

	* the US file is cumulative -- convert to daily cases / deaths (not using deaths here) ;
	proc sort data=UScases /*NODUPKEY*/; by fips date; run;

	data UScases (rename=(date=UScase_date));
	set UScases;
	by fips date;
	array T {2,2} _temporary_;;
	if first.fips then do;
		T[1,1]=0; T[1,2]=0; T[2,1]=0; T[2,2]=0;
	end;
	T[1,1]=cases;
	T[1,2]=deaths;
	cases=cases-T[2,1];
	deaths=deaths-T[2,2];
	T[2,1]=T[1,1];
	T[2,2]=T[1,2];
	run;

	proc sort data=UScases; by fips UScase_date; run;

	proc sql noprint;
	select put(min(UScase_date),date9.) into :min_rtdate from UScases;
	select put(max(UScase_date),date9.) into :max_rtdate from UScases;
	quit;

	%if &rtend= %then %do;
		%let rtend=&max_rtdate;
	%end;

	data _null_;
	call symputx("bad_start",("&min_rtdate"d>"&rtstart"d));
	call symputx("bad_end",("&max_rtdate"d<"&rtend"d));
	run;

	%if &bad_start OR &bad_end %then %do;
		%put :: The bounds of the rate data are not sufficient for your request;
		%put :: You requested: &rtstart to &rtend ... Actual data span &min_rtdate to &max_rtdate;
		%put :: SAS is quitting;
		%abort cancel;
	%end;

	data UScases;
	set UScases;
	by fips UScase_date;
	length daynum blockavg 8;
	array r {0:&LBstart} _temporary_;
	array rm {0:&LBend} _temporary_;
	retain daynum r rm;
	if first.fips then do;
		daynum=0;
		call missing(of r[*], of rm[*]);
	end;
	r[mod(daynum,dim(r))]=cases;
	rm[mod(daynum,dim(rm))]=cases;
	* average # of cases/day for N-day block extending from day -<LBstart> to day -<LBend> ;
	if daynum>&LBend then blockavg=(sum(of r[*])-sum(of rm[*]))/n(of r[*]); 
	daynum+1;
	run;

	* get county populations (overall, not VA) ;
	proc import out=copops dbms=csv replace
		datafile="&popfile";
	run;

	data copops (rename=(statefips=state));
	set copops (keep=state county popestimate2019);
	length fips $5;
	if county*1>0; * this removes the state-level summary rows ;
	statefips=fipstate(state*1); * produces two-letter state abbrev. ;
	fips=compress(put(state,z2.) || put(county,z3.));  * = 5-digit FIPS code ;
	drop state county;
	run;

	proc sort data=copops NODUPKEY; by fips; run;

	proc sql;
	create table county_covid_rates as
	select a.fips, a.UScase_date, 
	a.blockavg label="avg # of cases during &blocklen-day lookback lagged by &blocklag days", 
	b.popestimate2019 as fips_popUS, a.blockavg/b.popestimate2019*10000 as rt10kpop
	from
		UScases A
		inner join
		copops B
		on a.fips=b.fips;
	quit;

	proc univariate data=county_covid_rates noprint;
	WHERE "&rtstart"d<=UScase_date<="&rtend"d;
	var rt10kpop;
	output out=co_quints pctlpre=p pctlpts=20 40 60 80;
	run;

	data _null_;
	set co_quints;
	array p {*} p20 p40 p60 p80;
	do i=1 to dim(p);
		call symputx(compress('rt' || i), p[i]);
	end;
	run;

	proc format;
	value frtq
		low-<&rt1='rtQ1'
		&rt1-<&rt2='rtQ2'
		&rt2-<&rt3='rtQ3'
		&rt3-<&rt4='rtQ4'
		&rt4-high='rtQ5'
		;
	run;

	%if &joindata^= %then %do;
		proc sql;
		create table &joindata as
		select a.*, b.fips_popUS, b.blockavg, b.popestimate2019, b.rt10kpop
		from
			&joindata A
			left join
			county_covid_rates B
			on a.fips=b.fips and a.&joindata_datevar=b.UScase_date;
		quit;

		%put ::: county-level Covid-19 rates were added to &joindata;
	%end;

%MEND; *county_covid_rates();





