
/* 
	...playing around with an algorithm for suppressing 
	counts between 1 and 10 in a frequency table, assuming
	leftmost column and topmost row represent totals.
	
	Nov 2025
	J Smith
	
	WARNING: this is barely tested at all! **DO NOT USE**
	
	** the following has two cells that need suppression ;
	data raw;
	input tot grpA grpB grpC;
	cards;
	559 174 203 182
	111 53 48 10
	131 33 50 48
	104 14 44 46
	137 25 56 56
	76 49 5 22
	;
	run;
	
	======================================================== */

* one cell needs suppression, and because there are row and col totals
present, 3 additional cells (TBD) need to be masked ;
data raw;
input tot grpA grpB grpC;
cards;
559 174 200 185
111 53 45 13
131 33 50 48
104 14 44 46
137 25 56 56
76 49 5 22
;
run;

data _null_;
if 0 then set raw nobs=nobs;
array v {*} _numeric_;
array T {100} $32 _temporary_;
do i=1 to dim(v);
	T[i]=vname(v[i]);
end;
call symputx("colvars", catx(' ', of T[*]));
call symputx("C", dim(v));
call symputx("R", nobs);
stop;
run;

data suppressed;
set raw end=last;
array T {0:%eval(&C-1), 0:%eval(&R-1)} _temporary_;
array c {*} &colvars;
array xy {%sysevalf(&C*&R), 2} _temporary_;  /* 4*6 = 24 */
retain ns 0;
do i=1 to dim(c);
	T[i-1, _N_-1]=c[i];
	if 0 < c[i] < 11 then do;
		ns+1;
		xy[ns, 1]=i-1;
		xy[ns, 2]=_N_-1;
	end;
end;
if last then do;
	if ns=0 then goto mktbl;
	do i=1 to ns;
		x=xy[i, 1];
		y=xy[i, 2];
		* check if position x, y has already been set to missing 
		by a previous iteration - if so, go to next iteration of loop ;
		if T[x, y]=. then continue;
		minX=1;
		minY=1;
		minXloc=.;
		minYloc=.;
		do xloc=1 to hbound(T, 1);
			if xloc ^= x and T[xloc, y] / T[0, y] <= minX then do;
				minXloc=xloc;
				minX=T[xloc, y] / T[0, y];
			end;
		end;
		do yloc=1 to hbound(T, 2);
			if yloc ^= y and T[x, yloc] / T[x, 0] <= minY then do;
				minYloc=yloc;
				minY=T[x, yloc] / T[x, 0];
			end;
		end;
		* suppress the current value (x, y), the horizontal sibling (minXloc, y), 
		the vertical sibling (x, minYloc), and the diagonal sibling (minXloc, minYloc) ;
		T[x, y]=.; T[minXloc, y]=.; T[x, minYloc]=.; T[minXloc, minYloc]=.;
	end;
	mktbl:
	* output the suppressed table ;
	do y=0 to hbound(T, 2);
		do x=0 to hbound(T, 1);
			c[x+1]=T[x, y];
		end;
		output;
	end;
end;
keep &colvars;
run;

title "original (raw)";
proc print data=raw; run;

title "suppressed";
proc print data=suppressed; run;
title;

