

%let sitelist=ABCDEFGHIJKLMNOP;
%let global_start=01Jan2010;
%let global_end=31Dec2024;

data _null_;
call symputx("nmonths",intck('month',"&amp;global_start"d,"&amp;global_end"d)+1);
run;

data 
	site_p1 (keep=fmtname type start label)
	site_p2 (keep=fmtname2 type start label2
		rename=(fmtname2=fmtname label2=label))
	site_p3 (keep=fmtname3 type start label3
		rename=(fmtname3=fmtname label3=label))
	;
length fmtname fmtname2 fmtname3 $8 type $1 start $1 label label2 label3 8;
retain fmtname '$f1p' fmtname2 '$f2p' fmtname3 '$f3p' 
	type 'C' sites "&amp;sitelist";
do i=1 to length(sites);
	start=substr(sites,i,1);
	label=rand('normal')-10;
	label2=rand('normal')+0.5;
	label3=rand('normal')*rand('erlang',2);
	output;
end;
drop i sites;
run;

proc univariate data=site_p2 noprint;
histogram label;
run;

proc format cntlin=site_p1; run;
proc format cntlin=site_p2; run;
proc format cntlin=site_p3; run;

data sites;
length site $1 is_renewable 3 startdt enddt global_start global_end 4;
format startdt enddt global_: date9.;
retain sites "&amp;sitelist" global_start "&amp;global_start"d global_end "&amp;global_end"d;
global_range=global_end-global_start;
do i=1 to length(sites);
	site=substr(sites,i,1);
	nfac=int(rand('erlang',2)*10)+1;
	do f=1 to nfac;
		startdt=global_start+rand('integer',0,global_range);
		*ndays=min(int(rand('erlang',2)*(global_range/10)),global_end-startdt);
		*enddt=global_end-ndays;
		norm_date=(startdt-global_start)/global_range;
		xy=put(site,$f1p.)*(norm_date-put(site,$f2p.));
		lnxy=log(max(0.01,abs(xy)));
		if xy&lt;=0 then lnxy=lnxy*-1;
		explnxy=1+exp(lnxy);
		prob_rn=1/(1+explnxy);
		is_renewable=(rand('uniform')*0.5&lt;prob_rn);
		MW=rand('erlang',2)*250+((is_renewable=0)*rand('erlang',2)*100);
		ndays=(global_end-startdt);
		ndays+is_renewable*(rand('normal')-0.05*ndays);
		ndays+(0.15*ndays*put(site,$f3p.));
		enddt=min(global_end, startdt+ndays);
		output;
	end;
end;
drop i f nfac global_:;
run;

/*
proc univariate data=sites noprint;
histogram prob_rn;
run;
proc means data=sites nmiss; var prob_rn; run; */

proc sql;
select min(startdt) as first_start format=date9., 
max(enddt) as last_end format=date9.
from sites;
quit;

/*
proc print data=sites heading=v width=min; run; */

proc sort data=sites; by site startdt; run;

data sites;
set sites;
by site;
length monYR 4 
	onboard_N onboard_N_renewable onboard_MW onboard_MW_renewable 4
	pctN_renewable pctMW_renewable 8;
format monYR monyy7. pct: percent8.1;
array oN {&amp;nmonths} _temporary_;
array oNr {&amp;nmonths} _temporary_;
array oMW {&amp;nmonths} _temporary_;
array oMWr {&amp;nmonths} _temporary_;
if first.site then do;
	do i=1 to dim(oN);
		oN[i]=0; oNr[i]=0; oMW[i]=0; oMWr[i]=0;
	end;
end;
start_mo=intck('month',"&amp;global_start"d,startdt)+1;
end_mo=intck('month',"&amp;global_start"d,enddt)+1;
do i=start_mo to end_mo;
	oN[i]+1;
	oNr[i]+is_renewable;
	oMW[i]+MW;
	oMWr[i]+(MW*is_renewable);
end;
if last.site then do;
	do i=1 to dim(oN);
		monYR=intnx('month',"&amp;global_start"d,i-1);
		onboard_N=oN[i];
		onboard_N_renewable=oNr[i];
		onboard_MW=oMW[i];
		onboard_MW_renewable=oMWr[i];
		pctN_renewable=onboard_N_renewable/onboard_N;
		pctMW_renewable=onboard_MW_renewable/onboard_MW;
		output;
	end;
end;
keep site monYR onboard_: pct:;
run;

data sites;
set sites;


proc sgplot data=sites noautolegend;
series x=monYR y=pctMW_renewable / 
group=site smoothconnect curvelabel;
run;
