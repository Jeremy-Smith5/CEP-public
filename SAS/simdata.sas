
data test;
    format startdate date9.;
    do i = 1 to 10000; /* Adjust the number of observations as needed */
        startdate = '01JAN2010'd + floor(ranuni(12345) * ( '31DEC2024'd - '01JAN2010'd ));
        
        /* Normalize the date to a range, e.g., 0 to 1 */
        normalized_date = (startdate - '01JAN2010'd) / ('31DEC2024'd - '01JAN2010'd);
        
        /* Calculate the probability using a logistic function */
        prob_X = 1 / (1 + exp(-10 * (normalized_date - 0.5))); /* Adjust the parameters as needed */
        
        /* Generate the binary variable 'X' */
        Y = (ranuni(12345) < prob_X);
        
        output;
    end;
run;

proc sort data=test; by startdate; run;

proc print data=test (obs=20); run;

proc sql;
create table test as
select input(put(startdate,yymmn6.),8.) as ym, mean(Y) as Y
from test group by ym order by ym;
quit;

proc sgplot data=test noautolegend;
series x=ym y=Y / smoothconnect;
xaxis type=time;
run;

%let sitelist=ABCDEFGHIJKLMNOP;

data 
	site_p1 (keep=fmtname type start label)
	site_p2 (keep=fmtname2 type start label2
		rename=(fmtname2=fmtname label2=label))
	;
length fmtname fmtname2 $8 type $1 start $1 label label2 8;
retain fmtname '$f1p' fmtname2 '$f2p' type 'C' sites "&sitelist";



data test;
length site $1 is_renewable 3 startdt enddt global_start global_end 4;
format startdt enddt global_: date9.;
retain sites "&sitelist" global_start '01Jan2010'd global_end '31Dec2024'd;
global_range=global_end-global_start;
do i=1 to length(sites);
	site=substr(sites,i,1);
	nfac=int(rand('erlang',2)*10)+1;
	do f=1 to nfac;
		startdt=global_start+rand('integer',0,global_range);
		ndays=min(int(rand('erlang',2)*(global_range/10)),global_end-startdt);
		enddt=global_end-ndays;
		norm_date=(startdt-global_start)/global_range;
		prob_rn=1/(1+exp(-10*(norm_date-0.5)));
		is_renewable=(rand('uniform')<prob_rn);
		
		
		