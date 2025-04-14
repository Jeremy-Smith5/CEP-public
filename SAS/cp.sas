
/*
	PROGRAM: cp6.sas
	
	PURPOSE: Given an 'event'-level input dataset with columns:
	
		<PTID> (name of patient ID variable) -- this can be a character or numeric variable
		
		STARTDATE: the start date for evaluating the patient's exposure (exposure prior to this will be left-
			truncated to this date)
			
		ENDDATE: the end date for evaluating the patient's exposure (exposure after this will be right-
			truncated to this date)
			
		EVENT: a variable containing a character string that identifies the event, e.g., 'hosp_stay', 
			'aspirin', 'pneumonia'.  (** EACH STRING MUST ITSELF BE A USEABLE VARIABLE NAME ** - only letters,
			numbers and underscores - no spaces - must start with letter or underscore.  Keep it short.)
			
		EDATE: the date on which the event starts
		
		DAYS: the number of days for which the event should 'persist' in the output data
		
		BLACKOUT (optional): -1/0/1 -- this is for the special case in which an ongoing exposure should be 
			carried forward indefinitely while another exposure exists comcomitantly with it.  See this	
			section of the code below for further information.
			
		LABVALUE (optional): this can be used to carry forward the most recent value associated with the event, 
			e.g., a lab value or drug dose.  
		
		...create a counting process output with one row for each period of time during which all exposures
		are static.  This data structure is useful in many pharm-epi applications (e.g., evaluating treatment
		patterns) and in longitudinal analysis (e.g., PROC PHREG in SAS allows special syntax for counting 
		process input so that time-varying exposure can be evaluated).
					
		SAMPLE MACRO CALL:
			cp6(
				infile=fakept_events,
				ptid=BENE_ID,
				cleanup=1,
				usedates=1,
				datetime=0
				);
				
		PARAMETERS:
			INFILE:  name of input event-level dataset
			PTID:  name of variable that identifies a patient
			CLEANUP: 
				0: leave output in 'raw' format with negative values indicating days since last exposure 
					and positive values indicating number of days remaining.  
				1: change negative values to zero and positive values to 1 -- this is the usual format 
					and is the default setting.
			USEDATES: 
				0: treat the variables STARTDATE ENDDATE and EDATE as days relative to the patient rather
					than as calendar dates.  The output, then, will also have unformatted integers rather 
					than dates.  This is often the appropriate method for creating input for longitudinal 
					models, e.g., Cox regression.
				1: treat these variables (in the input and output) as dates -- this is the default setting.
			DATETIME: 
				0: assume all date variables represent dates and that the DAYS variable represents number 
					of days.  This is the default setting.
				1: assume all date variables represent datetimes and that the DAYS variable represents 
					number of seconds.
				
		OUTPUT: Temporary (WORK) dataset called CP with columns  (BE SURE TO SAVE A PERMANENT COPY AFTER THE 
			MACRO CALL!):
			
			<PTID> -- patient identifier
			STARTDATE -- same as input
			ENDDATE -- same as input unless modified by an 'OUTCOME_' variable as described below
			WINSTART -- start date of window for the current row (inclusive)
			WINEND -- end date of window for the current row (exclusive)
			LEN -- difference between WINSTART and WINEND (just short for 'length') in days or seconds (if 
				datetimes)
			<ARRAY OF EVENT VARIABLES> -- 0/1 indicators (assuming CLEANUP was set to 1 in the macro call) 
				to indicate whether exposure is currently 'on' or 'off' in the window.
			
		NOTES:
			There are two special kinds of events (IN THE INPUT, LEAVE DAYS AS MISSING FOR BOTH OF THESE): 
			
				- events with the prefix COHORT_ENTRY_, e.g., 'COHORT_ENTRY_AMI' - this creates a variable
				in the output dataset which stays 'on' for the remainder of follow-up and can be used to 
				partition the output data into lookback and followup periods.  
				
				- events with the prefix OUTCOME_, e.g., 'OUTCOME_DISCHARGE' - this causes follow-up to end at the 
				first such event for the person.  All other exposures will be right truncated to this date, and the
				patient's ENDDATE will be reset to this date.
				
			Datetimes can be used instead of dates if needed - be sure to format these input variables as datetimes,
			and set the <DATETIME> parameter to 1 in the macro call.
			
			'Artificial' events are sometimes useful, e.g., to create an indicator for first 90 days post AMI in 
			the output or to create a 'count' of admissions.  For example:
			
				data events;
				set events;
				OUTPUT;  * all the 'regular' events ;
				if event='AMI' then do;
					* post AMI 90-day indicator ;
					event='AMI_post90d';
					days=90;
					OUTPUT;
				end;
				if event='hosp_stay' then do;
					* post discharge 30-day indicator ;
					event='post_disch30d';
					edate=edate+DAYS;  * i.e., date of admission + length of stay = start date for post-discharge ;
					days=30;
					OUTPUT;
				end;
				if event='hosp_stay' then do;
					* create a variable that can be summed in the output ;
					event='admit_count';
					DAYS=1;	* to retain the ability to count events in the output dataset, make a 1-day event and then 
						sum the resulting variable in the output ;
					OUTPUT;
				end;
				run;
				
				* NOTE that the above steps don't replace the original events (hosp_stay, AMI and any others) - those are 
				kept by way of the first OUTPUT statement.	

			A separate, more primitive, version of this algorithm was re-written in base Python - contact Jeremy if needed.
			
			BY: Jeremy Smith (contact: jjeerreemmyy@gmail.com)
			
			LAST MODIFIED: Dec 2020
	
	======================================================================================================================= */

%MACRO CP(
	infile,
	ptid=PERSON_ID,
	cleanup=1,
	datetime=0,
	usedates=1
	);

	title;
	proc contents data=&infile noprint
		out=conts
		(keep=name type length where=(upcase(name) in (%upcase("&ptid"), "QTY", "BLACKOUT", "EVENT")));
	run;

	%let interv=1;
	%let datelen=5;
	%let datefmt=date9.;
	%if &datetime %then %do;
		%let interv=%sysevalf(24*60*60);
		%let datelen=7;
		%let datefmt=datetime.;
	%end;

	proc sql noprint;
	select max(len) into :maxtruelen from (select distinct length(compress(event)) as len from &infile);
	select length into :actuallen from conts where upcase(name)="EVENT";
	select (count(*)=0) into :nosuchvar from conts WHERE upcase(name)=upcase("&ptid");
	select (count(*)=0) into :noqty from conts WHERE upcase(name)="QTY";
	select (count(*)=0) into :noblkout from conts WHERE upcase(name)="BLACKOUT";

	create table _outcomes as select distinct &ptid, min(case when lowcase(substr(event,1,7))="outcome"
	then edate else . end) as first_outcome from &infile
	group by &ptid having first_outcome ne . order by &ptid;

	select (count(*)>0) into :anyoutcome from _outcomes;
	quit;

	%if &nosuchvar %then %do;
		%put ****** THE VARIABLE &PTID WAS NOT FOUND IN THE DATASET &INFILE **** ;
		%goto stopCP;
	%end;

	proc format;
	value ftype
		1='x'
		2='$'
		;
	run;

	data _null_;
	set conts;
	where upcase(name)=upcase("&ptid");
	call symput("ptlen",tranwrd(compress(put(type,ftype.) || length),"x",""));
	run;

	%if &anyoutcome %then %do;
		proc sort data=&infile out=CP; by &ptid; run;

		data CP;
		merge
			CP (in=A)
			_outcomes (in=B);
		by &ptid;
		IF A;
		* reset enddate to min of enddate, first_outcome where first_outcome ^= . ;
		if first_outcome ne . then enddate=min(enddate,first_outcome)+1;
		drop first_outcome;
		run;
	%end;
	%else %do;
		data CP;
		set &infile;
		run;
	%end;

	data CP (drop=bad_entry_date);
	set CP end=last;
	* if event is 'cohort_entry', create an absorbing state for this event, i.e.,
	exposure persists through enddate ;
	if lowcase(substr(event,1,12))="cohort_entry" then do;
		if edate<startdate or edate>enddate then bad_entry_date+1;
		days=enddate-edate+1;
	end; 
	if lowcase(substr(event,1,7))="outcome" then days=1;
	if last then call symputx("bad_entry_date",bad_entry_date);
	run;

	%if &bad_entry_date %then %do;
		%put *********************************************************************** ;
		%put "There were &bad_entry_date instances where cohort entry date was ";
		%put "outside the startdate/enddate bounds!";
	%end;

	data CP;
	set CP;
	%if &noqty %then %do;
		length qty 3;
		qty=1;
	%end;
	%if &noblkout %then %do;
		length blackout 3;
		blackout=0;
	%end;
	length event2 $%sysfunc(max(%sysfunc(compress(&actuallen)),%sysfunc(compress(%eval(&maxtruelen+5)))));
	if blackout=1 then event2=compress("_BLK_" || event);
	else event2=event;
	drop event;
	rename event2=event;

	* for events that occur before startdate, do the following:
		if event is still ongoing as of startdate, change the edate to
		startdate and modify qty and days to reflect the remainder.
		Otherwise, delete the event ;
	if qty<=0 then qty=1;
	if edate<startdate then do;
		if edate+days>=startdate then do;
			qty=qty*((days-(startdate-edate))/days);
			days=days-(startdate-edate);
			edate=startdate;
		end;
		else delete;
	end;
	* remove events that occur after enddate ;
	if edate>enddate then DELETE;
	format startdate enddate edate &datefmt;
	run;
	
	%let LEV=;

	proc sql noprint;
	select distinct event into :allkeys separated by " " from CP;
	select distinct lowcase(event) into :check separated by " " from CP;
	*select distinct compress("_LV_" || tranwrd(event,"LABEVENT_","")) into :allLABS separated by " " from CP;
	select distinct compress("_LV_" || event) into :allLABS separated by " " from CP;
	*select distinct compress("_LV_" || tranwrd(event,"LABEVENT_","")) into :LEV separated by " " from CP
		where upcase(substr(event,1,9))="LABEVENT_";
	select distinct compress("_LV_" || event) into :LEV separated by " " from CP
		where upcase(substr(event,1,8))="LABEVENT";
	quit;

	%let nolabs=0;
	%if &LEV= %then %do;
		%let nolabs=1;
	%end;

	%let ne=%sysfunc(countW(&allkeys));
	%let nchk=%sysfunc(countW(&check));
	%if &ne ne &nchk %then %do;
		%put ***** THERE ARE MULTIPLE VERSIONS OF AT LEAST ONE EVENT DUE TO INCONSISTENT USE OF CASE ***** ;
		%put ***** AS A RESULT, ALL EVENT VARIABLES WILL BE FORCED TO lowcase **************************** ;
		%let allkeys=&check;
		%let ne=&nchk;
	%end;

	/*
	%let ce=;
	%let dose=;
	%let pe=;
	%do i=1 %to &ne;
		%let ce=&ce ce_%scan(&allkeys,&i); / * ce: current exposure * /
		%let dose=&dose ds_%scan(&allkeys,&i);
		%let pe=&pe pe_%scan(allkeys,&i); / * pe: pre-existing exposure * /
	%end; */

	* at this point, there should be NO records where days=0 ;
	proc sql noprint;
	select count(*) into :nzero from CP where days<=0;
	quit;

	%if &nzero %then %do;
		%let nzero=%sysfunc(compress(&nzero));
		%put **************************************************************************************** ;
		%put *** WARNING: there were &nzero records with DAYS <=0 (or missing)! ;
		%put *** THESE RECORDS WILL BE DELETED! NOTE THAT THIS MAY REMOVE *PEOPLE* FROM YOUR DATA. ;
		%put **************************************************************************************** ;
		proc sql;
		create table zero as select * from CP where days<=0;
		quit;
		title "first 25 of &nzero obs where days<=0 - records will be excluded";
		proc print data=zero (obs=25) heading=v width=min; run;
		title;
		proc datasets lib=work memtype=data nolist nodetails; delete zero; run; quit;
	%end;

	proc sort data=CP;
	by &ptid event edate descending days;    
	run;

	data CP badrecs;
	set CP;
	by &ptid event edate;
	if first.edate then do;
		if days<=0 then output badrecs;
		else output CP;   /*** NOTE: will need to think about how to handle mult events of same type/same day */
	end;
	run;

	proc sql noprint; select count(*) into :nzero2 from badrecs; quit;

	title "sample of &nzero2 records deleted d/t DAYS<=0";
	proc print data=badrecs (obs=25) heading=v width=min; run;

	proc sort data=CP; by &ptid edate; run;

	* add a record to the bottom of each <PTID> that is beyond the persistence of the last event ;
	data CP;
	set CP;
	by &ptid edate;
	output;
	if last.&ptid then do; /* output ANOTHER (dummy) record */
		event="LASTREC";
		edate=enddate+50000*&interv;
		days=10*&interv;
		blackout=0;
		qty=1;
		output;
	end;
	run;

	* for each unique event type, get blackout category (0,1,2 - explained below) ;
	* do not allow >1 category per event type ;
	proc sql noprint;
	* confirm only one cat/event ;
	select (max(ncats)>1) into :toomanycats from
		(select distinct event, count(distinct blackout) as ncats
		from CP group by event);
	%if &toomanycats %then %do;
		%put ************************************************************************************* ;
		%put *** at least one event has >1 blackout category defined ***************************** ;
		%put *** SAS is quitting ***************************************************************** ;
		%put ************************************************************************************* ;
		%goto stopCP;
	%end;

	* list of event types with blackout category=1 ;
	select distinct event into :bc1 separated by " " from CP where blackout=1;
	quit;

	proc sort data=CP; by &ptid edate descending blackout descending days; run;

	/* NOTE: 
		- the variable 'blackout' exists on (or is automatically added to) then input dataset
			and is an indicator for the blackout property of the corresponding event:
				2: event causes blackouts for blackout=1 events
				1: event is blacked out by blackout=2 events
				0: event neither causes blackouts nor is blacked out by others

		- the variable 'blkout' (added as a retained variable in the next step) is an integer 
			value reflecting the number of days currently remaining in the blackout.  It
			is incremented and decremented in a way similar to the UPDT array variables.
			Increments occur whenever a new blackout=2 event is encountered and are equal
			to the greater of (the number of DAYS for that event) or (the current value of 
			blkout).
			Decrements occur at the output of each record
	********************************************************************************************** */

	data CP;
	set CP;
	by &ptid edate;
	length prev_edate prev_recdate &datelen;
	format prev_edate prev_recdate &datefmt;
	retain prev_edate;
	prev_recdate=lag(edate);
	if first.&ptid then do;
		prev_recdate=startdate;
	end;
	if first.edate then prev_edate=prev_recdate;
	run;

	proc sort data=CP;
	by &ptid edate descending blackout descending days;
	run;

	/*
	title "INPUT DATA - FIRST 50 OBS";
	proc print data=CP (obs=50) noobs; run;*/

	data CP (keep=&ptid startdate enddate winstart winend &allkeys &LEV);
	set CP;
	
	by &ptid edate;
	length winstart winend &datelen &allkeys &datelen &allLABS 6 this_event 3 blkdays mindt &datelen start_state nchanges 3;
	format mindt winstart winend &datefmt;
	retain &allkeys &allLABS prev_edate blkdays winstart nchanges;
	%if &nolabs %then %do;
		labvalue=.;
	%end;
	array events {*} &allkeys;
	array lv {*} &allLABS;
	array origstat {&ne} _temporary_;
	array origlab {&ne} _temporary_;
	array newstat {&ne} _temporary_;
	array newlab {&ne} _temporary_;
	*rownum=_N_;
	if first.&ptid then do;
		do i=1 to dim(events);
			events[i]=0;
			lv[i]=.;
			origstat[i]=0;
			origlab[i]=.;
			newstat[i]=0;
			newlab[i]=.;
		end;
		winstart=startdate;
	end;
	if first.edate then do;
		prev_recdate=prev_edate; /* prev_recdate is not retained */
		* if there is a blackout event, the longest for this edate will be at first.edate, so set blkdays here ;
		blkdays=(blackout=2)*days;
		* deal with any PRE-update changes -- these will all be ON-->OFF for all OFFs < edate ;
		** here, we just need to look at positive values that will have run out (become zero) before edate ;
		** this occurs wherever 0<events[i]<(edate-prev_edate) ;
		** find the earliest such OFF date (if any) ;
		do while (1);
			mindt=.;
			do i=1 to dim(events);
				if 0<events[i]<(edate-prev_edate) then do; /* note that this cannot be true at first.ptid */
					mindt=min(mindt,prev_edate+events[i]);
				end;
			end;
			if mindt=. then LEAVE;  * leave WHILE loop -- no PRE-update changes needed ;
			do i=1 to dim(events);
				newstat[i]=events[i]; /* status as of prev_edate */
				newlab[i]=lv[i];
				events[i]=origstat[i]; /* status as of winstart */
				lv[i]=origlab[i];
			end;
			* NOTE that prev_edate and winstart are only different the first time thru the loop ;
			** ... therefore, events=origstat for all subsequent iterations ;
			* events are updated as of mindt. output a record, update prev_edate, and continue the loop ;
			winend=MIN(mindt,enddate); /* not retained */
			OUTPUT;
			* update all events as of mindt ;
			do i=1 to dim(events);
				events[i]=newstat[i]-(mindt-prev_edate); /*status as of mindt*/
				if events[i]<=0 then lv[i]=.;
				origstat[i]=events[i];
				origlab[i]=lv[i];
			end;
			prev_edate=mindt; /* this gets retained */
			prev_recdate=mindt; /* ... this does not */
			winstart=winend; /* retained */
		end; * end WHILE loop ;
		* now, reset changes to 0 and prepare to make updates based on EDATE ;
		nchanges=0;
	end;

	* if this is the last record for this person, this is the dummy record - output a final record if winend<endate ;
	if last.&ptid then do;
		if winend<enddate then do;
			winend=enddate;
			OUTPUT;
		end;
		go to nextpt;
	end;

	do i=1 to dim(events);
		start_state=(events[i]>0);
		this_event=.;
		if upcase(event)=upcase(vname(events[i])) then do;
			this_event=1;
			lv[i]=labvalue;
			if events[i]=0 and origstat[i]>0 then do;
				nchanges=nchanges-1; /* turned off, then turned back on same date */
				start_state=1;
			end;
		end;
		events[i]=max(
			days*this_event /*missing, not zero, if not this event*/, 
			events[i]-(edate-prev_RECdate)
			)
			+
			((index(upcase(vname(events[i])),"_BLK_")>0)*last.edate*blkdays);
		events[i]=min(events[i],enddate-edate);
		if start_state>events[i] then lv[i]=.;
			/* NOTE if there is an overlap of blackout events, should probably output a warning */
		*nchanges=nchanges+(start_state^=(events[i]>0));
		nchanges=nchanges+max((start_state^=(events[i]>0)),(this_event*(labvalue>.)));
	end;
	if last.edate then do;
		if nchanges>0 then do; /* this will ALWAYS be true for the first unique event date for a given pt */
			do i=1 to &ne;
				newlab[i]=lv[i];
				newstat[i]=events[i];
				lv[i]=origlab[i];
				events[i]=origstat[i];
			end;
			winend=edate;
			OUTPUT;
			do i=1 to &ne;
				lv[i]=newlab[i];
				events[i]=newstat[i];
				origlab[i]=newlab[i];
				origstat[i]=newstat[i];
			end;
			winstart=edate;
		end;
		** NOTE that if no changes occurred, [origstat] ^= [events] ;
		else do;
			prev_edate=winstart; /* retained */
		end;
	end;
	nextpt:
	run;

	data CP;
	length &ptid &ptlen startdate enddate winstart winend LEN &datelen &allkeys &datelen;
	set CP;
	if winstart=winend then delete;
	LEN=winend-winstart;
	%if &usedates=0 %then %do;
		format startdate enddate winstart winend;
	%end;
	%if &cleanup=1 %then %do;
		array events {*} &allkeys;
		do i=1 to dim(events);
			events[i]=(events[i]>0);
		end;
		drop i;
	%end;
	run;

	%stopcp:
	title; 

%MEND; *CP();

/*
data test;
array e {8} $30 ("BetaBlock","ARB","CaChBlocker","labevent_serCal","labevent_Klev","hospstay","SNF","outcome_pneumonia");
array rx {10} _temporary_ (30,30,30,30,30,90,7,7,15,28);
array labs {6} _temporary_ (15,15,30,30,30,90);
array labvals {10} _temporary_ (60,72,79,82,83,83,85,93,108,145);
array hstay {20} _temporary_ (1,2,2,3,3,3,3,4,4,4,4,4,5,5,6,6,7,8,10,15);
array snfstay {15} _temporary_ (7,10,14,14,14,28,28,28,28,56,56,30,60,60,90);
length person_id 3 startdate enddate 5 event $30 edate 5 days 4;
format startdate enddate edate yymmddn8.;
do person_id=1 to 10;
	nrecs=abs(int(rannor(0)*100));
	event="cohort_entry_DMdx";
	edate="01Jan2006"d+int(rannor(0)*1000);
	startdate=edate-365;
	enddate=edate+abs(int(rannor(0)*1000));
	output;
	do r=1 to nrecs;
		event=e[int(ranuni(0)*dim(e))+1];
		if lowcase(event)="outcome_pneumonia" then event=e[int(ranuni(0)*dim(e))+1];
		if lowcase(event) in ('betablock', 'arb', 'cachblocker') then days=rx[int(ranuni(0)*dim(rx))+1];
		else if index(lowcase(event),'labevent') then do; 
			days=labs[int(ranuni(0)*dim(labs))+1];
			labvalue=labvals[int(ranuni(0)*dim(labvals))+1];
		end;
		else if lowcase(event)='hospstay' then days=hstay[int(ranuni(0)*dim(hstay))+1];
		else if lowcase(event)='snf' then days=snfstay[int(ranuni(0)*dim(snfstay))+1];
		if lowcase(event)='outcome_pneumonia' then edate=startdate+380+int(ranuni(0)*750);
		else edate=startdate+365+int(rannor(0)*1000);
		output;
		labvalue=.;
	end;
end;
keep person_id startdate enddate event edate days labvalue;
run; */

/*
options nodate nonumber formdlim='' pagesize=max;

data test;
infile cards dlm=',';
length person_id 3 startdate enddate 5 event $30 edate 5 days 4 labvalue 8;
input person_id startdate enddate event edate days labvalue;
cards;
1, 295, 390, drugA, 212, 30, .
1, 295, 390, cohort_entry_DM, 310, ., .
1, 295, 390, drugA, 299, 30, .
1, 295, 390, labevent_HgB, 315, 30, 157.2
1, 295, 390, labevent_HgB, 350, 30, 170.0
1, 295, 390, drugC, 320, 30, .
1, 295, 390, drugA, 323, 60, .
1, 295, 390, labevent_HgB, 365, 30, 148.8
;
run;
 
title "TEST input data - first 50 obs";
proc print data=test (obs=50) width=min; run;

%cp(test, usedates=0);

title "TEST output data - first 50 obs";
proc print data=cp (obs=50) width=min; run; */



