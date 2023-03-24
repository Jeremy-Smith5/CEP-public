/* 
Create an 'ascii' histogram based on a numeric variable from 
a SAS dataset.  This is primarily for use in environments
that do not support graphical windows and is being 
parameterized and launched by the Python script pysub.py (alias:
'histo').  Optionally, (distonly=1), just show distribution
of stated variable (with optional CLASS var) - this can be
used to help make trimming and binning decisions for the
histo itself.

Jeremy Smith -- 2019

============================================================= */

%MACRO aschisto(
	inds=, 
	var=, 
	nbins=,		/* this can be either 1) a single integer representing the desired number of bins, or 
			2) <low>/<high>/<by>, e.g., 10/150/5 for 10 to 150 by 5 -- option #1 does NOT work very well */
	trimlow=, 
	trimhigh=, 
	usrfmt=, 
	overwrite=0,
	domedian=0,
	incident_only=0,	/* IMPORTANT: setting to 1 assumes that you are producing a time series and that <VAR> is a date! */
	incident_ptvar=patientICN,
	y_interv=AUTO,
	convert_datetimes=1,
	trimlowval=,
	trimhighval=,
	narrow=0,
	WHERESTATE=,	/* blank for no where clause statement */
	distonly=0,
		CLASSstate=,  /* only applicable if distonly=1 */
	);

	%let fromtoby=0;

	%if &nbins= %then %do;
		%let nbins=10;
	%end;
	%else %do;
		data _null_;
		call symputx("nslash",countc("&nbins","/"));
		run;
		%if &nslash^=2 %then %do;
			%let nbins=10;
		%end;
		%else %do;
			%let nbins=%sysfunc(compress(&nbins));
			%let fromtoby=1;

			data _null_;
			call symputx("mps",scan("&nbins",1,"/"));
			call symputx("mpe",scan("&nbins",2,"/"));
			call symputx("mpb",scan("&nbins",3,"/"));
			run;

			%let trimlowval=&mps;
			%let trimhighval=&mpe;

			%let usrfmt=fmpufmt;

			proc format;
			value fmpufmt
			%do i=&mps %to &mpe %by &mpb;
				%let rb=%sysevalf(&i+&mpb);
				%if &rb<&mpe %then %do;
					&i-<&rb=&i
				%end;
				%else %do;
					&i-&rb=&i
				%end;
			%end;
			;
			run;
		%end;
	%end;

	%let note=;
	%let temp=_INDS;
	%if &overwrite %then %let temp=&inds;;

	%let temp=%upcase(&temp);
	%let inlib=WORK;
	%let indslib=WORK;
	%let indsname=&inds;
	%if %index(&temp,.) %then %do;
		%let inlib=%upcase(%scan(&temp,1,.));
		%let temp=%upcase(%scan(&temp,2,.));
	%end;
	%if %index(&inds,.) %then %do;
		%let indslib=%upcase(%scan(&inds,1,.));
		%let indsname=%upcase(%scan(&inds,2,.));
	%end;
	
	proc sql noprint;
	select format into :vfmt0 
	from dictionary.columns 
	where libname="&inlib" and upcase(memname)="&temp"
	and upcase(name)=upcase("&var");
	quit;

	%let ws_excl_n=;

	%if &overwrite=0 %then %do;
		data &temp;
		set &inds;
		&WHERESTATE;
		run;

		%let vexists=0;
		proc sql noprint;
		select count(*) into :vexists 
		from dictionary.columns 
		where libname="&inlib" and upcase(memname)="&temp"
		and upcase(name)=upcase("&var");
		quit;

		%if &vexists=0 %then %do;
			%put ::: the variable &var does not exist in &inds! ;
			proc print data=&inlib..&temp (obs=10) heading=v width=min; run;
			%abort cancel;
		%end;
	
		%if %length(&WHERESTATE) %then %do;
			proc sql noprint;
			select nobs into :nobs_temp
			from dictionary.tables 
			where libname="&inlib" and upcase(memname)="&temp";
			
			select nobs into :nobs_inds
			from dictionary.tables 
			where libname="&indslib" and upcase(memname)="&indsname";
			quit;

			data _null_;
			call symputx("ws_excl_n",&nobs_inds-&nobs_temp);
			run;

			%let ws_excl_n=%cmpres((WHERE statement excluded &ws_excl_n obs));
		%end;
		
		proc sql noprint;
		select format into :vfmt0 
		from dictionary.columns 
		where libname="&inlib" and upcase(memname)="&temp"
		and upcase(name)=upcase("&var");
		quit;

		%if &incident_only %then %do;
			%if &incident_ptvar= %then %let incident_ptvar=patientICN;;

			proc sql;
			create table &temp as
			select &incident_ptvar, min(&var) as &var %if &vfmt0^= %then %do; format=&vfmt0. %end;
			from &temp group by &incident_ptvar;
			quit;
		%end;

		%if &trimlowval^= or &trimhighval^= %then %do;
			%if &trimlowval^= %then %do;
				%if %sysfunc(anyalpha(&trimlowval)) %then %do;
					%let trimlowval="&trimlowval"d;
				%end;
			%end;
			%if &trimhighval^= %then %do;
				%if %sysfunc(anyalpha(&trimhighval)) %then %do;
					%let trimhighval="&trimhighval"d;
				%end;
			%end;
			
			data _null_;
			set &temp (keep=&var) END=LAST;
			length ltr htr 8;
			retain ltr 0 htr 0;
			%if &trimlowval^= %then %do;
				if &var<&trimlowval then ltr+1;
			%end;
			%if &trimhighval^= %then %do;
				if &var>&trimhighval then htr+1;
			%end;
			if last then do;
				call symputx("ltr",ltr);
				call symputx("htr",htr);
			end;
			run;
			
			%let note=;
			%if &ltr %then %do;
				%let note=&note lowtrim(val:&trimlowval dropped:&ltr);
				data &temp;
				set &temp;
				WHERE &var>=&trimlowval;
				run;
			%end;
			%if &htr %then %do;
				%let note=&note hightrim(val:&trimhighval dropped:&htr);
				data &temp;
				set &temp;
				WHERE &var<=&trimhighval;
				run;
			%end;
			%if %length(&note) %then %do;
				%let note=%sysfunc(strip(&note));
			%end;
		%end;
	%end;
	
	proc sql noprint;	

	select (type='char') into :ischar 
	from dictionary.columns 
	where libname="&inlib" and upcase(memname)="&temp"
	and upcase(name)=upcase("&var");
	quit;

	%if &convert_datetimes or &ischar %then %do;
		data &temp;
		set &temp;
		%if %index(%lowcase(&vfmt0),datetime) %then %do;
			&var=datepart(&var);
			%let vfmt0=date9.;
		%end;
		%if &ischar %then %do;
			_&var=&var*1;
			%let vfmt0=;
			drop &var;
			rename _&var=&var;
		%end;
		run;
	%end;
	%if &usrfmt^= and &usrfmt^='' %then %do;
		%if %lowcase(&usrfmt)=w %then %let usrfmt=weeku3;
		%if %lowcase(&usrfmt)=yw %then %let usrfmt=weeku5;
		%if %lowcase(&usrfmt)=m %then %let usrfmt=yymmn6;
		%if %lowcase(&usrfmt)=q %then %let usrfmt=yyq;
		%if %lowcase(&usrfmt)=y %then %let usrfmt=year;
		%let vfmt=&usrfmt;
		%if %index(&usrfmt,.)=0 %then %let usrfmt=&usrfmt..;  /* <<-- note that some formats have a dot before the end, e.g., comma14.2 */
	%end;
	%else %do;
		%let vfmt=&vfmt0;
		%if &vfmt= %then %let vfmt=best.;
	%end;
	
	%let vfmt=%lowcase(%trim(&vfmt));
	%if %index(&vfmt,.)=0 %then %let vfmt=&vfmt..;

	proc sql noprint;
	select count(*) into :anyNonInt
	from &temp
	WHERE int(&var)^=&var;
	quit;

	%if &distonly %then %do;
		proc means data=&temp noprint;
		&classstate;
		var &var;
		output out=stats
			nmiss=miss min=min p1=p1 p5=p5 mean=mean median=med p95=p95 p99=p99 max=max;
		run;

		%let ncl=0;
		%if %length(&classstate) %then %do;
			%let classstate=%cmpres(&classstate);
			%let ncl=%eval(%sysfunc(countW(&classstate,' '))-1);
			%if &ncl=1 %then %do;
				%let classv=%scan(&classstate,2,' ');
			%end;
		%end;

		data stats;
		set stats;
		drop _type_;
		rename _freq_=N
			%if &ncl=1 %then %do;
				&classv=LVL
			%end;
			;
		run;

		title "distribution for &var in dataset &inds";
		%if %length(&wherestate) OR %length(&classstate) %then %do;
			title2 "WHERE: &wherestate ... CLASS: &classstate";
		%end;
		proc print data=stats noobs width=min heading=v;
		format N comma15. min p1 p5 mean med p95 p99 max &vfmt;
		run;
		title; title2;
	%end;
	%else %do;

		proc univariate data=&temp noprint;
		var &var;
		output out=_lowhigh
			%if &trimlow^= or &trimhigh^= or &domedian %then %do;
				%let domedian=1;
				pctlpre=p
				pctlpts= 
					1, 50, 99
					%if &trimlow^=1 and &trimlow^=99 and &trimlow^= %then %do; ,&trimlow %end;
					%if &trimhigh^=1 and &trimhigh^=99 and &trimhigh^= %then %do; ,&trimhigh %end;
			%end;
			n=_n
			mean=_mean
			nmiss=_nmiss
			min=_min
			max=_max
			;
		run;
			
		%let median=.;
		%let p1=.;
		%let p99=.;
		
		data _null_;
		set _lowhigh;
		_mean=round(_mean,0.01);
		%if &trimlow^= %then %do;
			call symputx('low',p&trimlow);
		%end;
		%if &trimhigh^= %then %do;
			call symputx('high',p&trimhigh);
		%end;
		%if &domedian %then %do;
			call symput('median',put(p50,&vfmt));
			call symput('p1',put(p1,&vfmt));
			call symput('p99',put(p99,&vfmt));
		%end;
		call symputx('n',_n);
		call symput('mean',put(_mean,&vfmt));
		call symputx('nmiss',_nmiss);
		%if &fromtoby %then %do;
			call symputx('min',_min);
			call symputx('max',_max);
		%end;
		%else %do;
			call symput('min',put(_min,&vfmt));
			call symput('max',put(_max,&vfmt));
		%end;
		call symputx('minval',_min);
		call symputx('maxval',_max);
		run;
		
		%let n_lowtrim=-1;
		%let n_hightrim=-1;
		
		%if &trimlow^= or &trimhigh^= %then %do;
			data &temp;
			set &temp end=last;
			retain _lowtrim 0 _hightrim 0;
			%if &trimlow^= %then %do;
				if .<&var<&low then do;
					_lowtrim+1;
					DELETE;
				end;
			%end;
			%if &trimhigh^= %then %do;
				if &var>&high then do;
					_hightrim+1;
					DELETE;
				end;
			%end;
			if LAST then do;
				call symputx('n_trimlow',_lowtrim);
				call symputx('n_trimhigh',_hightrim);
			end;
			run;
		%end;

		%if &usrfmt^= and &usrfmt^='' /*and &anyNonInt=0*/ %then %do;
			proc sql;
			create table bins as
			select a._charminpt_, a._minpt_, round(a.n/b.tot*100,0.01) as _obspct_
			from 
				(select 
					%if %lowcase(&usrfmt)^=u %then put(&var,&usrfmt); 
					%else &var; 
				as _charminpt_, 
				min(&var) as _minpt_, count(&var) as n
				from &temp group by _charminpt_) A
				cross join
				(select count(&var) as tot from &temp) B
			order by a._charminpt_; 
			quit;

			data shell;
			do _minpt_=&minval to &maxval;
				_charminpt_=put(_minpt_,&usrfmt.);
				output;
			end;
			run;

			proc sort data=shell NODUPKEY; by _charminpt_; run;

			data bins;
			merge
				shell (in=A)
				bins (in=B drop=_minpt_)
				;
			by _charminpt_;
			IF A;
			if NOT B then _obspct_=0;
			run;

			proc sort data=bins; by _minpt_; run;
		%end;
		%else %do;	
			proc univariate data=&temp noprint;
			format &var &vfmt;
			histogram &var / 
				nendpoints=&nbins 
				outhistogram=bins
				;
			run;
		%end;
		
		data _null_;
		set bins;
		retain maxlen 0;
		call symputx('nobs',_N_);
		maxlen=max(maxlen,length(left(put(_minpt_,&vfmt))));
		call symputx('maxlen',maxlen);
		call symputx('height',max(_obspct_/100));
		run;
		
		%if &nobs>120 %then %do;
			%put :: Your histogram is too wide using the current bins - try making it less granular ;
			%abort cancel;
		%end;
		* if >60 bins, force the narrow version of the histogram to prevent wrapping ;
		%if &nobs>60 %then %let narrow=1;;

		%if &y_interv= or %lowcase(&y_interv)=auto %then %do;
			proc format;
			value fsetres
				0-<.15=0.005
				.15-<.30=0.01
				.30-<.60=0.02
				.60-<.75=0.05
				.75-<.90=0.10
				.90-high=0.20
				;
			run;

			data _null_;
			call symputx('y_interv',put(&height,fsetres.)*1);
			run;
		%end;

		%if &y_interv<0.001 %then %do;
			%let yres=8.2;
			%let ylen=6;
		%end;
		%else %if &y_interv<0.01 %then %do;
			%let yres=8.1;
			%let ylen=5;
		%end;
		%else %do;
			%let yres=8.0;
			%let ylen=4;
		%end;
		
		%let ylab=%sysfunc(compress(%sysfunc(repeat(_,&ylen-1))));
		
		data forhisto (keep=yax b0 b1-b&nobs);
		set bins end=LAST;
		length yax $&ylen b0 $1 b1-b&nobs %if &narrow %then $1; %else $2; n 3;
		label yax="&ylab";
		%do i=0 %to &nobs;
			label b&i='_';
		%end;
		array b {*} b1-b&nobs;
		array starts {&nobs} _temporary_;
		array pcts {&nobs} _temporary_;
		array lens {&nobs} _temporary_;
		retain n 0;
		n+1;
		starts[n]=_minpt_;
		pcts[n]=_obspct_/100;
		lens[n]=length(left(put(_minpt_,&vfmt)));
		if LAST then do;
			do _yax=1 to 0 by -&y_interv;
				yax=left(put(_yax,percent&yres));
				if index(yax,"(") then yax=left("0%");
				b0='|';
				anyhits=0;
				do i=1 to dim(b);
					if pcts[i]>=_yax then do; 
						if pcts[i]>0 then do; /* special case - do not want any bar when bin represents literally 0% */
							%if &narrow %then %do; b[i]='#'; %end;
							%else %do; b[i]='[]'; %end;
							anyhits=1;
						end;
					end;
				end;
				if anyhits then output;
			end;
			yax='';
			b0='';
			do i=1 to dim(b);
				%if &narrow %then %do; b[i]="_"; %end;
				%else %do; b[i]="__"; %end;
			end;
			output;
			do x=1 to &maxlen;
				do i=1 to dim(b);
					if x<=lens[i] then do;
						b[i]=left(substr(left(put(starts[i],&vfmt)),x,1));
						if b[i]="-" then b[i]=left("|");  * make a fake vertical negative sign ;
					end;
					else %if &narrow %then %do; b[i]=' '; %end; %else %do; b[i]='  '; %end;
				end;
				output;
			end;
		end;
		run;
		
		data _null_;
		%if &narrow %then %do;
			call symput('sqlab',repeat("_",%eval(&nobs-1)));
		%end;
		%else %do;
			call symput('sqlab',repeat("__",%eval(&nobs-1)));
		%end;
		run;

		data squashed;
		set forhisto;
		array b {*} b1-b&nobs;
		length sq $%eval(&nobs* %if &narrow %then 1; %else 2;);
		label sq="&sqlab";
		sq=cat(of b[*]);
		drop b1-b&nobs;
		run;
		
		%let foot=;
		%if &trimlow^= %then %do;
			%let foot=...# dropped below p%trim(&trimlow) (val=&low): %trim(&n_trimlow);
		%end;
		%if &trimhigh^= %then %do;
			%let foot=%trim(&foot)....# dropped above p%trim(&trimhigh) (val=&high): %trim(&n_trimhigh);
		%end;
		%let foot=%trim(%cmpres(&foot));
		%if &incident_only %then %do;
			title "histo for INCIDENT &var in dataset &inds";
		%end;
		%else %do;
			title "histo for &var in dataset &inds";
		%end;
		title2 %cmpres(%bquote(N:%trim(&n) MISS:%trim(&nmiss) MIN: &min 
			P1: &p1 MEAN: &mean MED: &median 
			P99: &p99 MAX: &max &ws_excl_n));
		%if &trimlow^= or &trimhigh^= %then %do;
			*footnote "%trim(&foot)";
			title3 "&foot";
		%end;
		%else %if &trimlowval^= or &trimhighval^= %then %do;
			title3 "&note";
		%end;
		proc print data=squashed /*forhisto*/ noobs width=min label; run;
		title; title2; title3; *footnote;

	%end; * not dist only ;

%MEND; *aschisto();


