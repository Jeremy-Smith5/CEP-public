

/*
	PURPOSE: confirm diagnoses by 1 inpt or 2 outpt occurrences with outpt
	events occuring at least <minspan> and not more than <maxspan> days
	apart.

	Jeremy Smith
	6 Nov 2015


	NOTES: input data must have a numeric binary variable called INPT that is used
	to distinguish inpatient from outpatient events.  Any record where inpt=1
	will be considered to have met the criteria based on just one appearance.  Inpt=0
	events require 2. The variable CLMTYPE here is just descriptive (and is passed
	below as the <eventsource> variable) - it does not need to correspond in any
	way with the value of INPT, though here it does.  The <eventsource> variable,
	if included in the call, will cause the macro to produce diagnostic output based on
	the values of this variable so that the user can see how many people met the
	criteria on each combination (e.g., 1 inpt dx, 2 RX fills, 2 ER visits, etc.). This
	variable should be a character variable and contain NO SPACES.  If included, the
	variable will remain in the output dataset (appearing twice for inpt=0-defined
	diagnoses: dx_source and OPsec_source and once as dx_source for inpt=1-defined
	diagnosses).  The variable can be omitted if no diagnostics are needed.  DXLAB (passed
	below as <splitbyvar>) identifies the level at which diagnoses are to be split
	up - the input data can contain any number of unique values of <splitbyvar>
	(here they're all 'cancer').  The macro will consider each level independently
	in assessing whether criteria are met, e.g., {cancer, diabetes, COPD}.  The
	output dataset will have binary variables (as well as dates and diagnostics)
	for each level separately.  <splitbyvar> can be made more granular or coarse
	as required by the study, for instance, the full 5 digit ICD-9 code could
	be used as <splitbyvar>.

	The call looks like this:
	%span(cancer,
		personvar=enrolid,
		datevar=dxdate,
		splitbyvar=dxlab,
		eventsource=clmtype,
		minspan=30,
		maxspan=365,
		sameyear=0,
		firstlast=FIRST,
		flipOP=0,
		personlevvars=
		)

	=========================================================================== */

%macro span(
	indata,  /* name of event level input dataset - can be permanent */
	personvar=enrolid, /* person identifier variable */
	datevar=svcdate,  /* name of event date variable on input dataset */
	splitbyvar=,  /* name of variable that describes the level by which events should be split up (max length=19) */
	eventsource=, /* name of CHARACTER variable that describes source of event data, e.g. Rx, Dx etc. - can be missing */
	minspan=30,  /* min # of days separating diagnoses */
	maxspan=365, /* max # of days separating diagnoses */
	keepOPfails=0, /* 1: keep first OP date (for each condition) for people who have only 1 OP dx and no IP */
	sameyear=0,  /* 0/1: force the 2 OP diagnoses to be in the same year */
	firstlast=,  /* FIRST/LAST: set to FIRST to keep earliest 2 OP diagnoses, LAST to keep most recent 2 */
	flipOP=0,  /* 0/1: swap the order of the 2 OP diagnoses --
		if set to 1 then:
			dx #1 is the later of the 2 when <FIRSTLAST>=FIRST
			dx #1 is the earlier of the 2 when <FIRSTLAST>=LAST.
		Default value is 0 (no flipping) */
	personlevvars=  /* PERSON-LEVEL variables, if any, present on the input dataset that should be kept in output */
	);

	%let firstlast=%upcase(&firstlast);
	%let sortopt=;
	%if &firstlast ne FIRST and &firstlast ne LAST %then %do;
		%put ERROR: <firstlast> must be either FIRST or LAST;
		abort cancel;
	%end;
	%else %if &firstlast=LAST %then %let sortopt=DESCENDING;

	%if &splitbyvar= %then %do;
		%put ERROR: <splitbyvar> cannot be left blank!;
		abort cancel;
	%end;
	%let keepeventsource=1;
	%if &eventsource= %then %do;
		%let keepeventsource=0;
		%let eventsource=evsource;
		%let evlen=4;
	%end;

	* get length and type of <personvar> ;
	proc contents data=&indata noprint out=WORK.indataconts; run;

	data _null_;
	set WORK.indataconts;
	WHERE upcase(name)=upcase("&personvar");
	if type=2 then call symput("pvlen",compress("$" || length));
	else call symputx("pvlen",length);
	run;

	proc sql;
	create table WORK.split as select distinct &splitbyvar from &indata order by &splitbyvar;
	title "# of levels for &splitbyvar";
	select count(*) into :nsplits from WORK.split;
	quit;

	title "split levels";
	proc print data=WORK.split; run;
	title;

	proc sort data=&indata
		out=WORK.everyone (keep=&personvar &personlevvars
			%if &keepeventsource %then &eventsource /* just to check var len below - then drop */;)
		NODUPKEY;
	by &personvar &personlevvars;
	run;

	proc sql noprint;
	select count(distinct &personvar) into :n_ppl_everyone from WORK.everyone;
	select count(*) into :n_obs_everyone from WORK.everyone;
	quit;

	%if &keepeventsource %then %do;
		proc sql;
		* get length of eventsource variable ;
		select length into :evlen from dictionary.columns
		where libname='WORK' and upcase(memname)='EVERYONE'
		and upcase(name)=upcase("&eventsource");
		%let evlen=%sysfunc(compress(&evlen));
		quit;

		data WORK.everyone;
		set WORK.everyone;
		drop &eventsource;
		run;
	%end;

	%if &n_ppl_everyone ne &n_obs_everyone %then %do;
		%put ERROR: not all vars in (&personlevvars) are person-level!;
		abort cancel;
	%end;

	%do split_i=1 %to &nsplits;

		data _null_;
		set WORK.split;
		if _N_=&split_i;
		call symput("split",compress(upcase(&splitbyvar)));
		run;

		data WORK.IP WORK.OP;
		set &indata;
		&splitbyvar=upcase(compress(&splitbyvar));
		* <splitbyvar> must be variable name-friendly (no spaces or special
			or special characters other than underscores.  And it cannot
			exceed 19 characters in length (because a 13-character prefix
			is added in the case of 'OPsec_source_<splitbyvar>') - >19 chars
			would cause this to exceed the 32-character max for variable names.
			If longer, write a message to the log and crash the program. ;
		&splitbyvar=compress(&splitbyvar,compress(&splitbyvar,"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"));
		if length(&splitbyvar)>19 then do;
			put "ERROR: SAS is quitting due to a level of splitbyvar (&split) exceeding 19 characters in length";
			ABORT CANCEL;
		end;

		IF &splitbyvar="&split";

		%if &keepeventsource=0 %then %do;
			length evsource $&evlen;
			evsource='NULL';
		%end;
		format &datevar yymmddn8.;
		keep &personvar &datevar inpt &eventsource;
		if inpt then output WORK.IP;
		else output WORK.OP;
		run;

		proc sql noprint;
		select (count(*)>0) into :anyop from WORK.OP;
		select (count(*)>0) into :anyip from WORK.IP;
		quit;

		%if &anyop %then %do;

			proc sort data=WORK.OP; by &personvar &sortopt &datevar &eventsource; run;

			data WORK.OP;
			set WORK.OP;
			by &personvar &sortopt &datevar &eventsource;
			IF first.&datevar; /* for a given event type, limit to one/day...
						note that the <eventsource> var can be used to
						assign a priority (based on sort order above) in case there are multiple
						sources for same event on same day, e.g., diabetes by Rx and Dx
						on 10 May 2015 */
			run;

			proc sql noprint;
			select max(n_dates) into :maxdates
			from
				(select distinct &personvar, count(*) as n_dates
				from WORK.OP group by &personvar);
			quit;

			%let maxdates=%sysfunc(compress(&maxdates));

			* transpose sorted dates horizontally, then iterate over the array like this:
			1-2, 1-3, 1-4, 1-5... , 2-3, 2-4, 2-5... 3-4, 3-5...
			until the first pair of dates that meet the criteria is found (if any) and output
			only these people to OP (one row per person) ;
			data
				WORK.OP (keep=&personvar inpt dx_date dx_source OPsec_date OPsec_source)
				WORK.failed_crit
				;
			set WORK.OP;
			by &personvar &sortopt &datevar;
			length dx_date 4 dx_source $&evlen OPsec_date 4 OPsec_source $&evlen;
			format dx_date OPsec_date yymmddn8.;
			label OPsec_date="&datevar for confirmation of OP dx by &minspan/&maxspan/&firstlast criteria";
			retain counter;
			array dates {&maxdates} _temporary_;  /* temp arrays are automatically retained */
			array evtypes {&maxdates} $&evlen _temporary_;
			if first.&personvar then do;
				do i=1 to &maxdates;
					dates[i]=.;
					evtypes[i]='';
				end;
				counter=0;
			end;
			counter+1;
			dates[counter]=&datevar;
			evtypes[counter]=&eventsource;
			if last.&personvar then do;
				startpos=1;
				dx_date=dates[1];
				dx_source=evtypes[1];
				do while (OPsec_date=. and startpos<counter);
					dx_date=dates[startpos];
					dx_source=evtypes[startpos];
					startpos+1;
					do i=startpos to counter;
						if (abs(dx_date-dates[i])>&maxspan)  /* dates are more than <maxspan> days apart */
							OR
							((year(dx_date)^=year(dates[i]))*&sameyear)	 /* dates are not in same year and <sameyear>=1 */
							then leave; /* leave inner loop */
						else if abs(dx_date-dates[i])>=&minspan then do;
							OPsec_date=dates[i];  /* these dates meet <minspan>/<maxspan>/<sameyear> crit. */
							OPsec_source=evtypes[i];
							leave; /* leave inner loop --> leave while loop */
						end;
					end;
				end;
				if OPsec_date=. then output WORK.failed_crit;
				%if &keepOPfails=0 %then else; output WORK.OP;
			end;
			run;

			%if &flipOP %then %do;
				data WORK.OP;
				set WORK.OP;
				length _temp 4 _temp2 $10;
				_temp=OPsec_date;
				_temp2=OPsec_source;
				OPsec_date=dx_date;
				OPsec_source=dx_source;
				dx_date=_temp;
				dx_source=_temp2;
				drop _temp _temp2;
				format dx_date OPsec_date yymmddn8.;
				run;
			%end;

			/*
			title2 "first 20 obs of OP records that failed the &minspan/&maxspan/&sameyear criteria";
			proc print data=failed_crit (obs=20); run;
			title2 "first 20 obs of OP records that PASSED the &minspan/&maxspan/&sameyear criteria";
			proc print data=OP (obs=20); run; */

		%end; *anyop;

		* stack inpt back with OP ;
		data WORK.ipop;
		set
			%if &anyip %then WORK.IP (rename=(&datevar=dx_date &eventsource=dx_source));
			%if &anyop %then WORK.OP;
			;
		label dx_date="&firstlast &datevar meeting 1 inpt or 2 outpt dx crit";
		%if &anyOP=0 %then %do;
			length OPsec_date 4 OPsec_source $10;
			OPsec_date=.; OPsec_source='';
		%end;
		run;

		proc sort data=WORK.ipop; by &personvar &sortopt dx_date descending inpt dx_source; run;

		data
			WORK.ipop (drop=event_type confirm_type rename=(
				dx_date=dx_date_&split OPsec_date=OPsec_date_&split inpt=inpt_&split
				dx_source=dx_source_&split OPsec_source=OPsec_source_&split))
			WORK.diagnostics (keep=inpt event_type confirm_type)
			;
		set WORK.ipop;
		by &personvar &sortopt dx_date descending inpt dx_source;  /* inpt event will beat out outpt if same day */
		length _done 3;
		retain _done;
		if first.&personvar then _done=0;
		*IF FIRST.&personvar;

		length event_type $40 confirm_type $21;
		event_type="&split";
		if OPsec_date>. then do;
			if dx_source>OPsec_source then confirm_type=lowcase(compress(OPsec_source || "_" || dx_source));
			else confirm_type=lowcase(compress(dx_source || "_" || OPsec_source));
		end;
		else confirm_type=upcase(compress(dx_source));
		if _done=0 and (inpt=1 or OPsec_date>. or last.&personvar) then do;
			_done=1;
			output;
		end;
		drop _done;
		run;

		proc sort data=WORK.ipop; by &personvar; run;
		proc append data=WORK.diagnostics base=WORK.all_diagnostics; run;

		title "SPLIT LEVEL: &split";

		proc sql;
		title2 "# of ppl in input dataset with 1+ occurrence";
		select count(distinct &personvar) from &indata where upcase(compress(&splitbyvar))="&split";
		title2 "# of ppl after 1 inpt / 2 outpt restriction applied";
		select count(distinct &personvar) from WORK.ipop;
		quit;

		data WORK.everyone;
		length &personvar &pvlen n_conf 3;
		merge
			WORK.everyone (in=A)
			WORK.ipop (in=B)
			;
		by &personvar;
		IF A;
		length conf_&split %if &keepOPfails %then opfail_&split; 3;
		label
			conf_&split="confirmed &splitbyvar=&split by 1 inpt/2 outpt &minspan/&maxspan/&sameyear/&firstlast crit"
			n_conf="total # of confirmed conditions"
			;
		conf_&split=(B=1);
		%if &keepOPfails %then %do;
			if dx_date_&split>. and inpt_&split^=1 then opfail_&split=0;
			if inpt_&split=0 and OPsec_date_&split=. then do;
				conf_&split=0; * only applicable if keepOPfails parameter is set to 1 ;
				opfail_&split=1;
			end;
		%end;
		n_conf=sum(of conf_:); /* this will get updated each time through the loop */
		run;

		*** THIS SHOULD NOT FIND DUPS **** ;
		proc sort data=WORK.everyone NODUPKEY; by &personvar; run;

		proc datasets lib=work memtype=data nolist nodetails;
		delete IP OP ipop;
		run; quit;

		%put :: SPAN MACRO FINISHED WITH: &split;

	%end; *split loop;

	options nolabel;
	title "final dataset";
	title2 "distr of numeric vars";
	proc means data=WORK.everyone mean sum;
	var conf_:;
	run;

	%if &keepeventsource %then %do;
		title "diagnostics";
		proc freq data=WORK.all_diagnostics;
		table event_type*confirm_type/missing; /* inpt-confirmed events will be upcase singles rather than lowcase pairs */
		run;
	%end;
	%else %do;
		data WORK.everyone;
		set WORK.everyone;
		drop dx_source_: OPsec_source_:;
		run;
	%end;
	
	title2 "first 20 obs";
	proc print data=WORK.everyone (obs=20) heading=vertical width=minimum; run;
	title; title2;

	options label;

%mend; *span();
