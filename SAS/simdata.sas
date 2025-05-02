
%let sitelist=ABCDEFGHIJKLMNOP;
%let global_start=01Jan2010;
%let global_end=31Dec2024;

%let stream=618810345;

%let xvar=monYR;
%let yvar=onboard_N;
%let bandvar=pctMW_rn;

%let yrlabXvar=onboard_N;
%let yrlabYvar=pctMW_rn;

data _null_;
call symputx("nmonths",intck('month',"&global_start"d,"&global_end"d)+1);
run;

data 
	site_p1 (keep=fmtname type start label)
	site_p2 (keep=fmtname2 type start label2
		rename=(fmtname2=fmtname label2=label))
	site_p3 (keep=fmtname3 type start label3
		rename=(fmtname3=fmtname label3=label))
	;
call streaminit(&stream);
length fmtname fmtname2 fmtname3 $8 type $1 start $1 label label2 label3 8;
retain fmtname '$f1p' fmtname2 '$f2p' fmtname3 '$f3p' 
	type 'C' sites "&sitelist";
do i=1 to length(sites);
	start=substr(sites,i,1);
	label=rand('normal')-10;
	label2=rand('normal')+0.5;
	label3=rand('normal')*rand('erlang',2);
	output;
end;
drop i sites;
run;

proc format cntlin=site_p1; run;
proc format cntlin=site_p2; run;
proc format cntlin=site_p3; run;

data sites;
call streaminit(&stream);
length site $1 is_rn 3 startdt enddt global_start global_end 4;
format startdt enddt global_: date9.;
retain sites "&sitelist" global_start "&global_start"d global_end "&global_end"d;
global_range=global_end-global_start;
do i=1 to length(sites);
	site=substr(sites,i,1);
	nfac=int(rand('erlang',2)*10)+1;
	do f=1 to nfac;
		startdt=global_start+rand('integer',0,global_range);
		norm_date=(startdt-global_start)/global_range;
		xy=put(site,$f1p.)*(norm_date-put(site,$f2p.));
		lnxy=log(max(0.01,abs(xy)));
		if xy<0 then lnxy=lnxy*-1;
		explnxy=1+exp(lnxy);
		prob_rn=1/(1+explnxy);
		is_rn=(rand('uniform')*0.5<prob_rn);
		MW=rand('erlang',2)*250+((is_rn=0)*rand('erlang',2)*100);
		ndays=(global_end-startdt);
		ndays+is_rn*(rand('normal')-0.05*ndays);
		ndays+(0.3*ndays*put(site,$f3p.));
		if rand('uniform')<0.2 then ndays=ceil(ndays*rand('uniform'));
		enddt=min(global_end, startdt+ndays);
		output;
	end;
end;
drop i f nfac global_:;
run;

proc sql;
select min(startdt) as first_start format=date9., 
max(enddt) as last_end format=date9.
from sites;
quit;

proc sort data=sites; by site startdt; run;

data 
	sites
	endvals
	;
set sites;
by site;
length monYR 4 
	onboard_N onboard_N_rn onboard_MW onboard_MW_rn 4
	pctN_rn pctMW_rn 8;
format monYR monyy7. pct: percent8.1;
array oN {&nmonths} _temporary_;
array oNr {&nmonths} _temporary_;
array oMW {&nmonths} _temporary_;
array oMWr {&nmonths} _temporary_;
if first.site then do;
	do i=1 to dim(oN);
		oN[i]=0; oNr[i]=0; oMW[i]=0; oMWr[i]=0;
	end;
end;
start_mo=intck('month',"&global_start"d,startdt)+1;
end_mo=intck('month',"&global_start"d,enddt)+1;
do i=start_mo to end_mo;
	oN[i]+1;
	oNr[i]+is_rn;
	oMW[i]+MW;
	oMWr[i]+(MW*is_rn);
end;
if last.site then do;
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
		output sites;
	end;
	output endvals;
end;
keep site monYR yrlab onboard_: pct: yrlab:;
run;

proc print data=endvals; run;

proc sql outobs=5;
create table top as
select site from endvals order by onboard_N DESC;
quit;

proc sql;
create table sites_top as
select a.*
from
	sites A
	inner join
	top B
	on a.site=b.site
order by site, monYR;
quit;

proc sql;
create table max as
select max(&bandvar) as maxBandvar, 
min(&yvar) as minYvar, max(&yvar) as maxYvar
from sites;

create table max_top as
select max(&bandvar) as maxBandvar, 
min(&yvar) as minYvar, max(&yvar) as maxYvar
from sites_top;
quit;

data sites;
if _N_=1 then set max;
set sites;
scale=(&bandvar/maxBandvar)*(maxYvar-minYvar)*0.05;
bndhi=&yvar+(scale/2);
bndlo=&yvar-(scale/2);
run;

data sites_top;
if _N_=1 then set max_top;
set sites_top;
scale=(&bandvar/maxBandvar)*(maxYvar-minYvar)*0.05;
bndhi=&yvar+(scale/2);
bndlo=&yvar-(scale/2);
run;

proc sgplot data=sites_top noautolegend;
series x=&xvar y=&yvar / group=site 
	smoothconnect curvelabel name="S";
band x=monYR lower=bndlo upper=bndhi / group=site
	modelname="S";
run;

proc sgplot data=sites_top noautolegend;
series x=&yrlabXvar y=&yrlabYvar / group=site smoothconnect curvelabel;
scatter x=yrlabX y=yrlabY / group=site markerattrs=(symbol=circle) datalabel=yrlab;
run;
