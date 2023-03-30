/*
	For a given SAS dataset containing localdrugSID, obtain the corresponding
	drug strength from the nationaldrug view in CDWWork.

	NOTE: if multiple strengths exist for a given localdrugSID, this macro
	is currently returning the maximum strength found.

	Also getting dose form (e.g., tablet, injection) from Dim.DosageForm

	J Smith
	29 Dec 2021 

	========================================================================= */

%MACRO rxstrength(
	inds=	/* name of SAS dataset containing localdrugSID */
	);

	proc sort data=&inds NODUPKEY out=localsids (keep=localdrugSID); by localdrugSID; run;

	data sdat.'#localsids'n (dbtype=(localdrugsid='BIGINT'));
	set localsids;
	WHERE not missing(localdrugSID);
	run;

	proc sql;
	connect to sqlsvr as sdat (datasrc=&PROJ &SQL_OPTIMAL);

	create table WORK.rxstrength as
	select localdrugSID, nationaldrugSID, strengthNumeric, dosageForm length=20
	from connection to sdat (
		select a.localdrugSID, c.nationaldrugSID, c.strengthNumeric, d.dosageForm
		from
			#localSIDs A
			INNER JOIN
			(select distinct localdrugSID, nationaldrugSID from [CDWWork].[Dim].[LocalDrug] 
			where nationaldrugSID is not NULL) B
			on a.localdrugSID=b.localdrugSID
			INNER JOIN
			(select distinct nationaldrugSID, strengthNumeric, dosageformSID from [CDWWork].[Dim].[NationalDrug]
			where strengthNumeric is not NULL) C
			on b.nationaldrugSID=c.nationaldrugSID
			LEFT JOIN
			(select distinct dosageformSID, dosageform from [CDWWork].[Dim].[DosageForm]
			where dosageform is not NULL) D
			on c.dosageformSID=d.dosageformSID
		)
	order by localdrugSID, strengthNumeric;

	execute (drop table if exists #localsids) by sdat;

	disconnect from sdat;
	
	quit;

	data rxstrength;
	set rxstrength;
	by localdrugSID strengthNumeric;
	IF LAST.strengthNumeric;
	run;

	proc sql noprint;
	select count(*) into :nraw from WORK.localSIDs;
	select count(*) into :nfinal from WORK.rxstrength;

	drop table WORK.localSIDs;
	quit;

	%let nraw=%sysfunc(compress(&nraw));
	%let nfinal=%sysfunc(compress(&nfinal));

	%let strpct=%sysfunc(compress(%sysevalf(&nfinal/&nraw*100)));
	
	%put ::: non-NULL Rx strength was found for &nfinal of &nraw unique localdrugSIDs (&strpct pct);

%MEND; *rxstrength();


