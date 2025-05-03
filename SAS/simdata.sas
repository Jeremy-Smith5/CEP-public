/*
	Make a simulated dataset containing basic information 
	on energy-producing facilities for several countries, 
	and then plot uptake in use of renewable sources.
	
	J Smith
	1 May 2025
	====================================================== */

%let cntrylist=ABCDEFGHIJKLMNOP;
%let global_start=01Jan2010;
%let global_end=31Dec2024;

%let stream=218810349;
%let ntop=5;

%let xvar=monYR;
%let yvar=onboard_N;
%let bandvar=pctMW_rn;

%let yrlabXvar=onboard_N;
%let yrlabYvar=pctMW_rn;

data _null_;
call symputx("nmonths",intck('month',"&global_start"d,"&global_end"d)+1);
run;

data 
	cntry_p1 (keep=fmtname type start label)
	cntry_p2 (keep=fmtname2 type start label2
		rename=(fmtname2=fmtname label2=label))
	cntry_p3 (keep=fmtname3 type start label3
		rename=(fmtname3=fmtname label3=label))
	;
call streaminit(&stream);
length fmtname fmtname2 fmtname3 $8 type $1 start $1 label label2 label3 8;
retain fmtname '$f1p' fmtname2 '$f2p' fmtname3 '$f3p' 
	type 'C' cntrys "&cntrylist";
do i=1 to length(cntrys);
	start=substr(cntrys,i,1);
	label=rand('normal')-10;
	label2=rand('normal')+0.5;
	label3=rand('normal')*rand('erlang',2);
	output;
end;
drop i cntrys;
run;

proc format cntlin=cntry_p1; run;
proc format cntlin=cntry_p2; run;
proc format cntlin=cntry_p3; run;

data cntrys (rename=(f=facility));
call streaminit(&stream);
length cntry $1 f is_rn 3 startdt enddt global_start global_end 4;
format startdt enddt global_: date9.;
retain cntrys "&cntrylist" global_start "&global_start"d global_end "&global_end"d;
global_range=global_end-global_start;
do i=1 to length(cntrys);
	cntry=substr(cntrys,i,1);
	nfac=int(rand('erlang',2)*10)+1;
	do f=1 to nfac;
		startdt=global_start+rand('integer',0,global_range);
		norm_date=(startdt-global_start)/global_range;
		xy=put(cntry,$f1p.)*(norm_date-put(cntry,$f2p.));
		lnxy=log(max(0.01,abs(xy)));
		if xy<0 then lnxy=lnxy*-1;
		explnxy=1+exp(lnxy);
		prob_rn=1/(1+explnxy);
		is_rn=(rand('uniform')*0.5<prob_rn);
		MW=rand('erlang',2)*250+((is_rn=0)*rand('erlang',2)*100);
		ndays=(global_end-startdt);
		ndays+is_rn*(rand('normal')-0.05*ndays);
		ndays+(0.3*ndays*put(cntry,$f3p.));
		if rand('uniform')<0.2 then ndays=ceil(ndays*rand('uniform'));
		enddt=min(global_end, startdt+ndays);
		output;
	end;
end;
drop i nfac global_: cntrys;
run;

title f="Times New Roman Uni" h=12pt 
	"simulated raw data - first 10 rows";
proc print data=cntrys (obs=10); run;

proc sort data=cntrys; by cntry startdt; run;

data 
	cntrys
	endvals
	;
set cntrys;
by cntry;
length monYR 4 
	onboard_N onboard_N_rn onboard_MW onboard_MW_rn 4
	pctN_rn pctMW_rn 8 dtOK 4;
retain nstarts dtOK;
format monYR dt3 monyy7. pct: percent8.1;
array oN {&nmonths} _temporary_;
array oNr {&nmonths} _temporary_;
array oMW {&nmonths} _temporary_;
array oMWr {&nmonths} _temporary_;
if first.cntry then do;
	do i=1 to dim(oN);
		oN[i]=0; oNr[i]=0; oMW[i]=0; oMWr[i]=0;
	end;
	nstarts=0;
	dtOK=.;
end;
start_mo=intck('month',"&global_start"d,startdt)+1;
end_mo=intck('month',"&global_start"d,enddt)+1;
nstarts+1;
if nstarts=3 then dtOK=intnx('month',"&global_start"d,start_mo);
do i=start_mo to end_mo;
	oN[i]+1;
	oNr[i]+is_rn;
	oMW[i]+MW;
	oMWr[i]+(MW*is_rn);
end;
if last.cntry then do;
	do i=1 to dim(oN);
		monYR=intnx('month',"&global_start"d,i-1);
		onboard_N=oN[i];
		onboard_N_rn=oNr[i];
		onboard_MW=oMW[i];
		onboard_MW_rn=oMWr[i];
		pctN_rn=onboard_N_rn/onboard_N;
		pctMW_rn=onboard_MW_rn/onboard_MW;
		yrlab=.;
		yrlabX=.;
		yrlabY=.;
		if month(monYR)=1 then do;
			yrlab=year(monYR);
			yrlabX=&yrlabXvar;
			yrlabY=&yrlabYvar;
		end;
		output cntrys;
	end;
	output endvals;
end;
keep cntry monYR yrlab onboard_: pct: yrlab: dtOK;
run;

proc sql outobs=&ntop;
create table top as
select cntry from endvals order by onboard_N DESC;

/*
create table top as
select cntry, max(onboard_N) as max_ObN
from cntrys group by cntry order by max_ObN DESC; */
quit;

proc sql;
create table cntrys_top as
select a.*
from
	cntrys A
	inner join
	top B
	on a.cntry=b.cntry
order by cntry, monYR;
quit;


title f="Times New Roman Uni" h=12pt 
	"simulated longitudinal data - first 10 rows";
proc print data=cntrys_top (obs=10); run;

proc sql;
create table max as
select max(&bandvar) as maxBandvar, 
min(&yvar) as minYvar, max(&yvar) as maxYvar
from cntrys;

create table max_top as
select max(&bandvar) as maxBandvar, 
min(&yvar) as minYvar, max(&yvar) as maxYvar
from cntrys_top;
quit;

data cntrys;
if _N_=1 then set max;
set cntrys;
scale=(&bandvar/maxBandvar)*(maxYvar-minYvar)*0.05;
bndhi=&yvar+(scale/2);
bndlo=&yvar-(scale/2);
run;

data cntrys_top;
if _N_=1 then set max_top;
set cntrys_top;
scale=(&bandvar/maxBandvar)*(maxYvar-minYvar)*0.05;
bndhi=&yvar+(scale/2);
bndlo=&yvar-(scale/2);
run;

ods graphics on / width=8in height=6in;

proc sgplot data=cntrys_top noautolegend;
title f="Times New Roman Uni" h=14pt "Active sites by month / country, 2010-2024";
series x=&xvar y=&yvar / group=cntry 
	smoothconnect curvelabel curvelabelattrs=(family="Roman" size=12)
	name="S";
band x=monYR lower=bndlo upper=bndhi / group=cntry
	modelname="S";
xaxis display=(nolabel) valuesrotate=vertical fitpolicy=rotate
	interval=year notimesplit type=time;
yaxis label="% of installations active" values=(0 to 40 by 5) grid;
footnote justify=right f="Times New Roman Uni" 
	h=10pt italic "simulated data - band width corresponds to current % renewable" ;
run;

proc sgplot data=cntrys_top noautolegend;
*WHERE .<dtOK<=monYR;
title f="Times New Roman Uni" h=14pt 
	"Active sites vs. use of renewable energy, by country";
format yrlabY percent8.0;
series x=&yrlabXvar y=&yrlabYvar / group=cntry smoothconnect 
	curvelabel curvelabelattrs=(family="Roman" size=14 color=black weight=bold)
	curvelabelpos=end;
scatter x=yrlabX y=yrlabY / group=cntry 
	markerattrs=(symbol=circle) datalabel=yrlab;
xaxis label="# of installations active" values=(0 to 40 by 5);
yaxis label="% of MW output from renewables" min=0 max=1;
footnote justify=right f="Times New Roman Uni" 
	h=10pt italic "simulated data" ;
run;
