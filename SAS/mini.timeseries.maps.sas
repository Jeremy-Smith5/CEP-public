/*
PROGRAM: mini.timeseries.maps.sas

Given a shapefile-like SAS dataset containing coordinates and a corresponding dataset
with matching geographic units containing rates (or similar numeric values) by geographic
area and time period, create a panel of maps that shows change over time.

vhawrjsmithj
03 Jul 2024

NOTES:
	- this is intended for making a series of small, low-resolution maps.  For high-
		resolution maps, use timeseries.maps.sas.
	- if <timedata> is not provided, it will be created automatically from all unique
		values of <timevar> contained in <ratedata> - note this will cause the 
		image to be missing maps for any values of <timevar> that are absent
		from <ratedata>.
	- an example call with simulated rate data and a market-level shapefile is shown
		below this macro.

========================================================================================= */

%MACRO minimaps(
	shapedata=,	/* name of shapefile-like SAS dataset -- include libname if not WORK - should include <xvar>, <yvar>, <geovar> [, <densityvar>] */
	timedata=,	/* OPTIONAL - dataset containing all time periods to be plotted - should contain same <timevar> as used in <ratedata> */
	ratedata=,	/* dataset containing <timevar>, <geovar>, <ratevar>, i.e., the rate data by time and geography */
	xvar=X,		/* <shapedata> variable to be plotted on x-axis (usually, longitude) */
	yvar=Y,		/* <shapedata> variable to be plotted on y-axis (usually, latitude) */
	densityvar=DENSITY, /* OPTIONAL - if provided, the variable in <shapedata> the corresponds to the resolution */
	densityvar_max=1,   /* OPTIONAL - if provided along with <densityvar>, the value of <densityvar> corresponding to the maximum resolution to be used */ 
	geovar=,	/* e.g., VISN */
	ratevar=,	/* <ratedata> variable containing the rate or other numeric value to be represented by <colors> */
	timevar=,	/* <ratedata> variable containing the time bins, e.g., YEAR */
	colors=,	/* color sequence formatted as keywords (lightblue blue purple ...) or hex values (cxFFFFFF cxFF0000 ...) for low->high values of <ratevar> */
	nwide=,		/* number of COLUMNS to use in output panel - for example, for 6 years of data, enter nwide=3 to create a panel 3 maps across and 2 maps down */
	outgraph=minimaps,  /* name of output .tiff image file */
	outdir=&CWD,	/* output path for <outgraph> */
	panel_width=12,	/* width of panel, in inches */
	panel_height=8, /* height of panel, in inches */
	panel_title=,	/* OPTIONAL: title for panel (not individual maps) */
	panel_title_fontsize=10,  /* OPTIONAL: if title provided, the title font size in points */
	panel_orientation=landscape  /* orientation of panel -- landscape or portrait */
	);

	%if %length(&timedata)=0 %then %do;
		proc sql;
		create table timedata as
		select distinct &timevar from &ratedata
		order by &timevar;
		quit;
		
		%let timedata=timedata;
	%end;

	data shapes;
	length rowid 8;
	set &shapedata %if %length(&densityvar) %then %do; (WHERE=(&densityvar<=&densityvar_max)) %end;;
	rowid=_N_;
	run;

	proc sql;
	create table minimaps as
	select a.&timevar, a.rowid, a.&Xvar, a.&Yvar, a.&geovar, b.&ratevar
	from
		(select t.&timevar, s.rowid, s.&Xvar, s.&Yvar, s.&geovar
		from
			&timedata T
			cross join
			shapes S
		) A
		LEFT join
		&ratedata B
		on a.&geovar=b.&geovar and a.&timevar=b.&timevar
	order by a.&timevar, a.rowid;
	
	drop table shapes;
	quit;

	%include "&COMMON/plot_syntax.sas";

	%plot_syntax(
		outgraph_imagefmt=tiff,  	/* extension: tiff, html, png, jpeg -- pdf may work also */
		outgraph=&outgraph, 		/* name for output image (no extension) */
		gwidth=&panel_width,		/* graph width in inches */
		gheight=&panel_height,		/* graph height in inches */
		graphdir=&outdir,		/* output path for graph */
		orientation=&panel_orientation,	/* portrait or landscape */
		otheroptions=,		/* enclose in %str( ), e.g., %str(dataskinmax=1000 antialiasmax=1000) - no semicolons */
		listingoptions=,	/* enclose in %str( ) -- options to be added to ODS LISTING statement, if any */
		outgraph_title=&panel_title,		/* title -- can be left blank */
			outgraph_title_font=Times New Roman,	/* font for title */
			outgraph_title_height=&panel_title_fontsize.pt		/* font size for title */
		);


	proc sgpanel data=minimaps NOAUTOLEGEND;
	PANELBY &timevar / novarname onepanel columns=&nwide noheaderborder border uniscale=all;
	polygon x=&Xvar y=&Yvar id=&geovar / FILL colorresponse=&ratevar colormodel=(&colors);
	colaxis display=(nolabel noticks novalues);
	rowaxis display=(nolabel noticks novalues);
	run;

	ods listing close;
	ods listing;

%MEND; *minimaps();

/* 
* example call ;

libname sf "&COMMON/shapefiles/county/fromVA";

%let outdir=%str(/cifs3/vhacdwfpcfs02/vhawrjsmithj/Projects/ord_dallelucca_202310004d);

proc sql;
create table rates as
select distinct market 
from sf.NO_PHI_MARKET_SHAPEFILE_REDPRJ
order by market;
quit;

data rates;
set rates;
length year 3 rthosp 8;
chg=1+(ranuni(0)*7+1)/100;
if ranuni(0)<0.2 then chg=chg*-0.5;
rthosp=ranuni(0)*5;
do year=2018 to 2023;
	rthosp=rthosp*chg;
	if ranuni(0)<0.1 then chg=chg*-1;
	output;
end;
drop chg;
run;

%minimaps(
	shapedata=sf.NO_PHI_MARKET_SHAPEFILE_REDPRJ,
	timedata=,
	ratedata=rates,
	xvar=X,
	yvar=Y,
	densityvar=DENSITY,
	densityvar_max=1,
	geovar=MARKET,
	ratevar=rthosp,
	timevar=year,
	colors=cxffffff cx1e90ff cx0302fc cx2a00d5 cx63009e cxa1015d cxd80027 cxfe0002,
	nwide=3,
	outgraph=example_maps,
	outdir=%str(/cifs3/vhacdwfpcfs02/vhawrjsmithj/Projects/ord_dallelucca_202310004d),
	panel_width=12,
	panel_height=8,
	panel_title=%str(Simulated hospitalization rate by VISN-market - 2018-2023),
	panel_title_fontsize=10,
	panel_orientation=landscape
	);

*/


