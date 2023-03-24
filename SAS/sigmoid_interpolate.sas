
/*
	Given an input dataset, a variable name that represents
	an x-coordinate, and a space-separated list of y variables,
	add <STEPS> records between each row of the input data
	so that the resulting set of x-y* coordinates interpolate
	a sigmoid ('S'-like) curve connecting the original x-y*
	coordinates.  This can be used, for ex., to create paths
	in a Sankey-like diagram.  

	NOTES: 
	the number of steps will correspond to the resulting
	resolution of the sigmoid curve. Values between 10 and 30
	are recommended (default=20).
	
	The input dataset must have at least 2 records. It will not
	be modified by this macro.

	The output dataset (for, e.g., plot input) will be called 
	WORK.sig and will have the same order (indicated by rowid)
	and same x-y* variable names as the input dataset.  

	J Smith
	Nov 2020

	========================================================== */

%macro sig(inds, xvar, yvarlist, steps=20);

	proc sql noprint;
	select count(*) into :dsrecs from &inds;
	quit;

	%let dsrecs=%sysfunc(compress(&dsrecs));
	%if &dsrecs<2 %then %goto insuffrecs;	

	%let sp1=%eval(&steps+1);
	
	%let yvarlist=%cmpres(&yvarlist);
	
	%do yv=1 %to %sysfunc(countW(&yvarlist,' '));
	
		%let yvar=%scan(&yvarlist,&yv,' ');
		
		data sig&yv (rename=(_x=&xvar _y=&yvar));
		length rowid srcrow targrow 8;
		set &inds (keep=&xvar &yvar) END=LAST;
		array xy [&dsrecs,2] _temporary_;
		xy[_N_,1]=&xvar;
		xy[_N_,2]=&yvar;
		if LAST then do;
			rowid=0;
			array sigx {&sp1} _temporary_;
			array sigy {&sp1} _temporary_;
			do i=2 to dim(xy,1);
				srcrow=i-1;
				targrow=i;
				diffx=xy[i,1]-xy[i-1,1];
				diffy=xy[i,2]-xy[i-1,2];
				do s=1 to dim(sigx);
					sigx[s]=(-&steps/2)+(s-1);
				end;
				do s=1 to dim(sigy);
					sigy[s]=1/(1+exp(-sigx[s]));
				end;
				* translate sigx and sigy to actual coordinates ;
				do s=1 to (dim(sigx)-1);
					sigx[s]=xy[i-1,1]+(s-1)*(diffx/&steps);
				end;
				sigx[dim(sigx)]=xy[i,1];
				do s=1 to (dim(sigy)-1);
					sigy[s]=xy[i-1,2]+sigy[s]*diffy;
				end;
				sigy[dim(sigy)]=xy[i,2];
				do s=1 to (dim(sigx)-1);
					rowid+1;
					_x=sigx[s];
					_y=sigy[s];
					output;
				end;
			end; *i loop;
			rowid+1;
			srcrow=&dsrecs-1;
			targrow=&dsrecs;
			_x=xy[dim(xy,1),1];
			_y=xy[dim(xy,1),2];
			output;
		end;
		keep rowid srcrow targrow _x _y;
		run;
		
		proc sort data=sig&yv; by rowid; run;
		
		%if &yv=1 %then %do;
			data sig;
			set sig&yv;
			run;
		%end;
		%else %do;
			data sig;
			merge
				sig
				sig&yv (keep=rowid &yvar)
				;
			by rowid;
			run;
		%end;
		
		proc datasets lib=work memtype=data nolist nodetails; 
		delete sig&yv;
		run; quit;
		
		proc sort data=sig; by rowid; run;
	
	%end; *yvarlist loop;

	%insuffrecs:

%mend; *sig();

/* 
*example call (for simplicity, run this in SAS EG - otherwise, modify 
to specify an output destination for SGPLOT);

%include '/data/prod/common/WRJ_macros/sigmoid_interpolate.sas';

data test;
infile cards dlm=',';
length x y1 y2 y3 y4 8;
input x y1 y2 y3 y4;
cards;
10, 17, 14, 28, 22
20, 25, 22, 5, 3
30, 8, 5, 18, 12
40, 12, 9, 25, 22
50, 30, 27, 34, 28
;
run;

%sig(test, x, y1 y2 y3 y4, steps=20);

proc sgplot data=sig noautolegend;
band x=x upper=y1 lower=y2 / transparency=0.5;
band x=x upper=y3 lower=y4 / transparency=0.5;
run; 

*/
