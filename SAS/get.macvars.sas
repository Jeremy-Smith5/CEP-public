
/*

	Collect scope, name and value of all macro variables in dictionary.macros
	and either show changes in all existing MV since last check or just show
	those newly created since last check.  

	Note this macro only produces output when isPost is set to 1 and the macro
	has previously been run with isPost set to 0.

	J Smith
	24 Feb 2023

	============================================================================== */

%macro getMacVars(incl_autos=0, isPost=0, newMVonly=1);

	proc sql noprint;
	select count(*) into :hasPriorMV from dictionary.tables where libname='WORK' and upcase(memname)='CURRMV0';
	select count(*) into :hasCurrMV from dictionary.tables where libname='WORK' and upcase(memname)='CURRMV1';
	quit;

	%if &hasPriorMV=0 and &isPost=1 %then %do;
		%put ::: sorry, the getMacVars macro must be run with isPost=0 prior to a run with isPost=1;
		%goto exitMV;
	%end;
	%if &hasCurrMV=1 and &isPost=1 %then %do;
		proc datasets lib=work memtype=data nolist nodetails; 
		DELETE currMV0; 
		CHANGE currMV1=currMV0;
		run; quit;
	%end;

	proc sql;
	create table currMV&isPost as
	select scope, name, value as value_&isPost 
	from dictionary.macros
	WHERE name NOT in ('HASPRIORMV', 'HASCURRMV')
	%if &incl_autos=0 %then %do;
		and scope^='AUTOMATIC'
	%end;
	order by scope, name;
	quit;

	%if &isPost %then %do;
		%if &newMVonly %then %do;
			proc sql;
			create table keepMV as
			select scope, name from currMV1 
			EXCEPT 
			select scope, name from currMV0
			order by scope, name;
			quit;
		%end;
		%else %do;
			proc sql;
			create table keepMV as
			select * from
				(
				select scope, name form currMV0
				union
				select scope, name from currMV1
				)
			order by scope, name;
			quit;
		%end;

		proc sql;
		create table MVchanges as
		select a.scope, a.name, %if &newMVonly=0 %then mv0.value_0 length=75; mv1.value_1 length=75
		from
			keepMV A
			%if &newMVonly=0 %then %do;
				left join
				currMV0 mv0
				on a.scope=mv0.scope and a.name=mv0.name
			%end;
			left join
			currMV1 mv1
			on a.scope=mv1.scope and a.name=mv1.name
		order by a.scope, a.name;
		quit;

		title "changes to macro variables since last check %if &newMVonly %then -- SHOWING NEWLY CREATED MV ONLY;";
		proc print data=MVchanges noobs width=min; run;

		proc datasets lib=work memtype=data nolist nodetails; delete currMV0 keepMV MVchanges; run; quit;
	%end;

	%exitMV:

%MEND; *getMacVars();


