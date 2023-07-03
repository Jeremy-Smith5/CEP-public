/*
Basic algorithm for calculating percent of days covered at the 
patient level by time interval of length <INTERV>, where first 
interval begins at date of earliest fill for patient and is 
calculated for <NINTERV> intervals or until censoring (here, 
just DOD). Fills are 'stacked' on top of each other.

The first step is just creating a fake input data step, 'FILLS'
for testing. Edate is date of fill and days is days supply.

J Smith 
Jul 2023

NOTE: this is only intended for use with a single drug. For 
multiple drugs, use cp6 (counting process) macro and add 
separate events for each interval to the input dataset.

============================================================== */

* create a fake input dataset ;
data fills;
length patientICN 5 dod edate days 5;
format edate date9.;
array T {5} _temporary_ (30, 30, 30, 90, 10);
do i=1 to 100;
	patientICN=1000+i;
	edate='01Jan2020'd + int(ranuni(0)*100);
	slack=int(rand('erlang', 2)*10);
	dod=.;
	if ranuni(0)<0.15 then dod=edate+int(ranuni(0)*730);
	do while (1);
		days=T[int(ranuni(0)*dim(T)+1)];
		output;
		edate+max(1,int(days+rand('normal')*5))+slack;
		if ranuni(0)<.05 then leave;
	end;
end;
drop i;
run;

title 'first 50 obs of fake input data';
proc print data=fills (obs=30); run;

proc sort data=fills; by patientICN edate; run;

%let interv=90;  * number of days in interval ;
%let ninterv=12; * number of intervals ;

* for each patient, calculate percent of days covered by drug for each interval ;
data pdc_ptlev;
set fills;
by patientICN edate;
length currend 5 currpos 3 bndry 3 pdc_p1-pdc_p&ninterv 5;
retain currend currpos pdc_:;
array p {*} pdc_:;
array ends {&ninterv} _temporary_;
array D {&ninterv} _temporary_;
if first.patientICN then do;
	currend=.;
	currpos=1;
	bndry=0;
	do i=1 to dim(p);
		p[i]=0;
		bndry+&interv;
		ends[i]=edate+bndry;
		D[i]=min(&interv,max(0/(DOD>.),&interv-(ends[i]-DOD)));
	end;
end;

currend=max(currend, edate);
do while (days>0);
	currend+1;
	days=days-1;
	if currend>ends[dim(ends)] or .<dod<=currend then leave;
	do while (ends[currpos]<=currend and currpos<dim(ends));
		currpos+1;
	end;
	p[currpos]+1;
end;

if last.patientICN then do;
	do i=1 to dim(p);
		p[i]=p[i]/D[i];
	end;
	output;
end;
keep patientICN pdc_:;
format pdc_: percent8.1;
run;

title 'first 30 obs of output -- one row/patient';
proc print data=pdc_ptlev (obs=30) heading=v width=min; run;
title;
