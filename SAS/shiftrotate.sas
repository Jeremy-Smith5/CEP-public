/* 
	Given a shapefile-like dataset containing X, Y vertices for polygons (each identified
	by an ID variable <IDVAR> and optional LABEL variable <LABELVAR>, 
	shift and/or rotate the vertices to a new position as defined by XSHIFT, YSHIFT, 
	CENTER_X_NEW, CENTER_Y_NEW, ROTATE.  

	XSHIFT and YSHIFT are assumed to be positive unless a '-' is included.  ROTATE (degrees)
	should be positive for counter-clockwise rotation or negative for clockwise.

	If KEEP_PRE is set to 1, the original vertices will be stacked with the shifted vertices
	and the value of the ID variable will be increased by 0.1 for the shifted set.

	To move points relative to original position, use variables XSHIFT and YSHIFT.
	
	Otherwise, provide variables CENTER_X_NEW and CENTER_Y_NEW on input file to center a shape 
	at a specific set of coordinates.
	
	vhawrjsmithj

	Oct 2022

	<INDATA> contains:
		<IDVAR> (must be integer) -- this identifies a given polygon (defined over 
  			multiple rows)
  		X - x-coordinate
    		Y - y-coordinate
		<LABELVAR> -- this is optional - if omitted, a new variable LAB will be created 
			that is just the character representation of the value in <IDVAR>
		ROTATE: a positive or negative number - if missing value, will be reset to 0
		SCALE: a floating point number >0 by which to resize the polygon - e.g., 0.6 will 
			make the polygon 60% of its original size.  2.5 will make it 2.5x larger.
			If missing, this defaults to 1.
		XSHIFT: relative amount to move polygon on X axis - if missing, will be reset to 0
		YSHIFT: relative amount to move polygon on Y axis - if missing, will be reset to 0
		CENTER_X_NEW: optional - if present (and non-missing), polygon will be moved to 
			this absolute position on the X axis
		CENTER_Y_NEW: optional - if present (and non-missing), polygon will be moved to 
			this absolute position on the Y axis

	EXAMPLE CALL:
		/data/prod/common/WRJ_macros/test.shiftrotate.sas

  	UPDATE 26 Mar 2024 - added an example call for animating a set of vertices and displaying 
   	with SGPLOT POLYGON statement.

	======================================================================================== */

%MACRO shiftrotate(
	indata=,
	idvar=segment,  /* IMPORTANT: this is assumed to be an integer! */
	labelvar=,
	keep_pre=1,
	othvars=	/* list of extra vars (aside from ID SEG LAB X Y), if any, to keep from <INDATA> */
	);

	proc sort data=&indata out=preSR; by &idvar; run;
	
	data preSR;
	length n 8;
	set preSR;
	by &idvar;
	if first.&idvar then n=0;
	n+1;
	run;
	
	%if &labelvar= %then %do;
		%let labelvar=lab;
		
		data preSR;
		set preSR;
		length &labelvar $12;
		&labelvar=put(&idvar,8.);
		run;
	%end;
	
	proc sql noprint;
	select max(n) into :maxn from (select &idvar, count(*) as n from preSR group by &idvar);
	quit;

	proc contents data=&indata noprint out=conts; run;

	%let newcenter=0;
	%let hasscale=0;

	proc sql noprint;
	select (count(*)=2) into :newcenter from conts where upcase(name) in ('CENTER_X_NEW', 'CENTER_Y_NEW');
	select (count(*)=1) into :hasscale from conts where upcase(name)='SCALE';
	quit;
	
	data SR (rename=(rnum=n));
	set preSR (rename=(x=x0 y=y0));
	by &idvar;
	array xs {&maxn} _temporary_;
	array ys {&maxn} _temporary_;
	if first.&idvar then do;
		call missing(of xs[*], of ys[*]);
	end;
	xs[n]=x0;
	ys[n]=y0;
	if last.&idvar then do;
		pi=constant('pi');
		* this is the current center of the polygon ;
		cntrX=mean(of xs[*]);
		cntrY=mean(of ys[*]);
		%if &newcenter %then %do;
			if center_x_new>. OR center_y_new>. then do;
				* new absolute coordinates were provided for this polygon ;
				if center_x_new>. then xshift=center_x_new-cntrX;
				if center_y_new>. then yshift=center_y_new-cntrY;
			end;
		%end;
		%if &hasscale=0 %then %do;
			length scale 8;
		%end;
		if xshift=. then xshift=0;
		if yshift=. then yshift=0;
		if rotate=. then rotate=0;
		if scale=. then scale=1;
		length anymove 3;
		anymove=0;
		if scale^=1 or xshift or yshift or rotate then anymove=1;
		do rnum=1 to n;
			if missing(xs[rnum]) or missing(ys[rnum]) then continue;
			x0=xs[rnum];
			y0=ys[rnum];
			if anymove then do;
				* calculate rotated values for x and y ;
				x=cos(rotate*pi/180) * (x0-cntrX) - sin(rotate*pi/180) * (y0-cntrY) + cntrX;
				y=sin(rotate*pi/180) * (x0-cntrX) + cos(rotate*pi/180) * (y0-cntrY) + cntrY;
				* adjust size of polygon by scale factor ;
				x=(x-cntrX)*sqrt(scale)+cntrX;
				y=(y-cntrY)*sqrt(scale)+cntrY;
				* shift x and y ;
				x=x+xshift;
				y=y+yshift;
			end;
			else do;
				x=x0;
				y=y0;
			end;
			output;
		end;
	end;
	keep &idvar &labelvar rnum x y xshift yshift rotate anymove scale %if &newcenter %then center_x_new center_y_new; &othvars;
	run;

	%if &keep_pre %then %do;
		proc sql;
		create table preSR as
		select a.*, 1 as anymove length=3 from 
			preSR A 
			inner join 
			(select distinct &idvar from SR where anymove=1) B 
			on a.&idvar=b.&idvar
		union all
		select a.*, 0 as anymove length=3 from 
			preSR A
			inner join
			(select distinct &idvar from SR where anymove=0) B
			on a.&idvar=b.&idvar
		order by &idvar, n;
		quit;

		data SR;
		set
			preSR (in=A)
			SR (in=B WHERE=(anymove=1))
			;
		if A then do;
			if anymove then &labelvar=compress(&labelvar || "_orig");
		end;
		if B then do;
			&idvar=&idvar+0.1; /* see note in macro def - original values should be integers! */
		end;
		run;
	%end;
			
%MEND; *shiftrotate();

/* 
*** THE FOLLOWING IS AN EXAMPLE CALL ADDED 26 MAR 2024 - 
here, creating a simple shapefile with instructions to move up
and to the right, shrinking to 95% of original size.  The animate
macro defined below then repeats this process N times and displays
with SGPLOT.   */

data VTshape;
infile cards dlm=',' dsd truncover firstobs=1;
length seg 3 slab $2 x y rotate scale xshift yshift /*center_x_new center_y_new*/ 8;
input seg slab x y rotate scale xshift yshift /*center_x_new center_y_new*/;
cards;
1,VT,3,3,5,0.95,20,20
1,VT,10,3,5,0.95,20,20
1,VT,12,18,5,0.95,20,20
1,VT,14,25,5,0.95,20,20
1,VT,3,25,5,0.95,20,20
1,VT,3,3,5,0.95,20,20
;
run;

data orig;
set VTshape;
run;

%macro animate(indata=, ntimes=, randomize=0);
			
	proc datasets lib=work memtype=data nolist nodetails;
	SAVE &indata orig;
	run; quit;
	
	proc sql noprint; select count(*) into :shaperecs trimmed from &indata; quit;

	%do move=1 %to &ntimes;

		%if &move>1 %then %do;
			data 
				prior (drop=rotate scale xshift yshift 
					rename=(_ro=rotate _scale=scale _xs=xshift _ys=yshift))
				&indata (drop=n anymove rotate scale xshift yshift 
					rename=(_ro=rotate _scale=scale _xs=xshift _ys=yshift))
				;
			set SR;
			retain _ro _scale _xs _ys;
			if _N_<=&shaperecs then output prior;
			else if _N_=&shaperecs+1 then do;
				_ro=rotate; _scale=scale; _xs=xshift; _ys=yshift;
				%if &randomize %then %do;
					do while (1);
						_ro=rand('normal')*50;
						_scale=rand('normal')+1;
						_xs=rand('normal')*5;
						_ys=rand('normal')*5;
						if _scale>0.1 then leave;
					end;
				%end;
			end;
			if _N_>&shaperecs then output &indata;
			run;
			
			proc append data=prior base=all_priors; run;
			
			proc datasets lib=work memtype=data nolist nodetails;
			SAVE &indata all_priors orig;
			run; quit;
		%end;

		%shiftrotate(
			indata=&indata,
			idvar=seg,  /* IMPORTANT: this is assumed to be an integer! */
			labelvar=slab,
			keep_pre=1,
			othvars=	/* list of extra vars (aside from ID SEG LAB X Y), if any, to keep from <INDATA> */
			);
			
	%end;
	
	proc append data=SR base=all_priors; run;
		
	proc sgplot data=all_priors NOAUTOLEGEND;
	polygon X=x Y=y id=seg / label=slab fill outline;
	run;
	
%mend; *animate();

options mprint;
%animate(indata=VTshape, ntimes=20, randomize=1);
%animate(indata=orig, ntimes=20, randomize=0);
