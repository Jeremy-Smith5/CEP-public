

/*
	IMPORTANT: this macro should be run as a first step in a SAS program and should only be run
	if calling the program from the command line (in Bash) using SASGSUB syntax as follows:

	* first (if not previously done) create an alias (here, called 'sas') in your ~/.bashrc file and
	source that file (source ~/.bashrc).  Alias should look like this:

 	alias sas='sasgsub -GRIDSASOPTS "(-sysparm $PWD)" -GRIDWAITRESULTS -GRIDSUBMITPGM'

	Then, SAS programs can be run as follows from the command line -- the -sysparm $PWD part is what
	is used by the mkglobals macro to create various global macro variables pertaining to the current
	ORD project and currently running SAS program / log ;


	*******************************************
	sas test.sas &
	******************************************* 

	The SAS Grid Manager will automatically notify you when the program finishes running.

	Create the following global macro variable to be used by the calling program: 
	
	SASPROG: 	full path of SAS program
	SASLOG:  	full path of SAS log

	-------------  the following are only applicable if the calling program is in an 'ord_*' directory ------

	PROJROOT:	full path of project 
	PROJ:		project only (without preceding path)
	DFLTLIB: 	the SAS libname referencing the Dlft schema for this project on SQL Server
	
	Jeremy Smith
	22 Jan 2018

	=====================================================================================  */
%macro endsas;
	%put ::: Program stopped due to 'endsas'! ;
	endsas;
%mend;

%macro mkglobals;
	options nodate nonumber varinitchk=warning;

	%if %symexist(SYSPARM) %then %do;

		%let mkvars=%eval((%symexist(sasprog)+%symexist(saslog)+%symexist(projroot)+%symexist(proj)+%symexist(dfltlib)+%symexist(pdrive))=0);

		%if &mkvars %then %do;

			%let isPATH=%index(&sysparm,%str(/));

			%if &isPATH %then %do;

				%global SASPROG SASPATH SASLOG PROJROOT PROJ DFLTLIB PDRIVE CWD;

				%let SASPROG=&sysparm/%scan(&sysprocessname,-1,%str(/));
				%let SASPATH=&SASPROG;
				%let SASLOG=%substr(&sasprog,1,%length(&sasprog)-4).log;
				%let PROJROOT=- N/A -;
				%let PROJ=- N/A -;
				%let DFLTLIB=- N/A -;
				%let PDRIVE=- N/A -;
				%let CWD=&sysparm;

				%let isORD=%index(&sysparm,%str(/ord_));

				%if &isORD %then %do;

					%let PROJROOT=%substr(&sysparm,1,&isORD+%index(%substr(&sysparm,&isORD+1),%str(/)));
					%let PROJ=%scan(&projroot,-1,%str(/));
					/*
					%if "%substr(&PROJROOT,%length(&PROJROOT),1)"="/" %then %do;
						%let PROJROOT=%substr(&PROJROOT,1,%length(&PROJROOT)-1);
					%end;*/
					proc sql noprint;
					select libname into :DFLTLIB from dictionary.libnames where upcase(engine)="SQLSVR"
					and upcase(path)=upcase("&proj") and upcase(sysvalue)="DFLT";
					quit;

					%let DFLTLIB=%sysfunc(compress(&DFLTLIB));

					* P Drive location must have correct case - unfortunately, there is not a generic way
					to translate the lowercase PI name to proper case because there are no spaces to 
					delineate words, e.g., no way to automatically translate 'youngxu' to 'YoungXu'. For
					now, just directly deal with the YoungXu situation and assume all others can be 
					converted correctly with the propcase() function. ;
					%if %index(&PROJ,youngxu) %then %do; %let PIname=YoungXu; %end;
					%else %do; %let PIname=%sysfunc(propcase(%scan(&PROJ,2,_))); %end;
					%let pathend=ord_%lowcase(&PIname._%scan(&PROJ,3,_));
					%let PDRIVE=/cifs3/vhacdwfpcfs02/&sysuserid/Projects/ORD_&PIname._%upcase(%scan(&PROJ,3,_)); 
					/*%let PDRIVE=/cifs3/vhacdwfpcfs02/&sysuserid/Projects/&pathend;*/
					%macro xnote(xfile, xcomment=);
						data xnote;
						length note $250;
						note="FILE:    		&xfile"; output; 
						note="CREATED: 		&sysdate9 &systime"; output;
						note="PROGRAM: 		&sasprog"; output;
						note="LOG:		&saslog"; output;
						note="PROGRAMMER:	&sysuserid"; output;
						%if %length(xcomment) %then %do;
							note="&xcomment"; output;
						%end;
						run;

						proc export data=xnote dbms=xlsx replace
							outfile="&xfile";
						sheet="NOTES";
						run;
					%mend;	

				%end;

				%put =================  global macro variables from mk.globals.sas ============================== ;
				%put :: SASPROG/SASPATH:&sasprog;
				%put :: SASLOG: 	&saslog;
				%put :: PROJROOT: 	&projroot;
				%put :: PROJ: 		&proj;
				%put :: DFLTLIB:	&dfltlib;
				%put :: PDRIVE: 	&pdrive;
				%put :: CWD: 		&cwd;
				%put ============================================================================================ ;
	
				libname cwd "&CWD";

			%end;

		%end;

	%end;

%mend; *mkglobals;

%mkglobals


