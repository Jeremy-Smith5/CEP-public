
## variable overlap -- usage for voverlap()
function vovr_usage() {
	echo ' '
	echo 'variable overlap USAGE:'
	echo '...valid flags are: -f (<file>), -v (<var>), -w (a: all, s: same, d: diff) -n (narrow=true)'
	echo '...use ":" in file and var names for wildcards, e.g., :dgns: or op:base:'
	echo '...file arg is REQUIRED - if files cannot be captured with wildcards, specify each independently with multiple -f args'
	echo '...if var is not specified, this function will return all variables'
	echo '...-w argument specifies WHICH results to show: "a" for all (default), "s" for same across all files, "d" for any difference'
	echo '...-n argument sets narrow=true - the function will not display separate columns for each input file name (default: false)'
	echo ' '
}

## show overlap of variables across 2+ SAS datasets
## see voverlap_usage() above for usage / documentation
function voverlap() {
	local OPTIND flag files vars whichdisp narrow
	while getopts 'f:v:w:n' flag; do
		case "${flag}" in
			f) files+="${OPTARG} " ;;
			v) vars+="${OPTARG} " ;;
			w) whichdisp="${OPTARG}" ;;
			n) narrow='true' ;;
			*) vovr_usage 
			   return ;;
		esac
	done
	shift $((OPTIND - 1))

	fwhere=''
	files="${files//:/%%}"
	IFS=' ' read -a flist <<< "${files}"
	nfiles=${#flist[@]}
	if [ $nfiles == 0 ]; then
		echo '...you must supply 1 or more filenames!'
		vovr_usage
		return 
	else
		fwhere='WHERE (upcase(memname) like upcase("'
		fclean=$(echo "${flist[0]}" | cut -d'.' -f 1)
		fwhere+="${fclean}"
		fwhere+='")'
		fnum=1
		while [[ $fnum < $nfiles ]]; do
			fwhere+=' or upcase(memname) like upcase("'
			fclean=$(echo "${flist[$fnum]}" | cut -d'.' -f 1)
			fwhere+="${fclean}"
			fwhere+='")'
			fnum=$((fnum + 1))
		done
		fwhere+=')'
	fi

	vwhere=''
	if [ "$vars" != "" ]; then
		vars="${vars//:/%%}"
		IFS=' ' read -a vlist <<< "${vars}"
		nvars=${#vlist[@]}
		vwhere='and (upcase(name) like upcase("'
		vwhere+="${vlist[0]}"
		vwhere+='")'
		vnum=1
		while [[ $vnum < $nvars ]]; do
			vwhere+=' or upcase(name) like upcase("'
			vwhere+="${vlist[$fnum]}"
			vwhere+='")'
			vnum=$((vnum + 1))
		done
		vwhere+=')'
	fi

	dispwhere=''
	if [ "$whichdisp" != "a" ] && [ "$whichdisp" != "" ]; then
		dispwhere='where upcase(vtype)=upcase("'
		dispwhere+="${whichdisp}"
		dispwhere+='")'
	fi
	
	#echo "files: '${fwhere}' ... vars: '${vwhere}' ... whichdisp: '$dispwhere'"

	echo "options mprint nodate nonumber nocenter formdlim=' ' pagesize=max;" > comline_$USER.sas;
	echo "libname here '$PWD';" >> comline_$USER.sas;
	echo '%include "/data/prod/common/WRJ_macros/mk.globals.sas";' >> comline_$USER.sas
	echo '%include "/data/prod/common/WRJ_macros/overlap.sas";' >> comline_$USER.sas
	echo "%macro chkvars;" >> comline_$USER.sas
	echo "%global dslist;" >> comline_$USER.sas
	echo "proc sql noprint; select lowcase(memname) into :dslist separated by ' ' from dictionary.tables" >> comline_$USER.sas
	echo "$fwhere" >> comline_$USER.sas
	echo "and libname='HERE'; quit;" >> comline_$USER.sas
	echo "%let nds=%sysfunc(countW(&dslist,' '));" >> comline_$USER.sas
	echo "%do i=1 %to &nds;" >> comline_$USER.sas
	echo "  %let f=%scan(&dslist,&i,' ');" >> comline_$USER.sas
	echo "  proc sql;" >> comline_$USER.sas
	echo '  create table &f as select lowcase(name) as vname from dictionary.columns where libname="HERE" and lowcase(memname)="&f"' >> comline_$USER.sas 
	if [ "$vwhere" != "" ]; then
		echo "  $vwhere" >> comline_$USER.sas
	fi
	echo "  order by vname;" >> comline_$USER.sas
	echo "  quit;" >> comline_$USER.sas
	echo "%end;" >> comline_$USER.sas
	echo "%mend;" >> comline_$USER.sas
	echo "%chkvars;" >> comline_$USER.sas

	echo '%overlap(dslist=&dslist, byvars=vname);' >> comline_$USER.sas
	echo 'proc sort data=chk_overlap SORTSEQ=LINGUISTIC (NUMERIC_COLLATION=on); by vname;  run;' >> comline_$USER.sas;
	echo "proc print data=chk_overlap heading=v width=min;" >> comline_$USER.sas
	if [ "$dispwhere" != "" ]; then
		echo "$dispwhere;" >> comline_$USER.sas
	fi
	if [ "$narrow" == 'true' ]; then
		echo "var vname inds vtype;" >> comline_$USER.sas
	fi
	echo "run;" >> comline_$USER.sas
	
	rm -f comline_$USER.lst
	rm -f comline_$USER.log
	sasgsub -GRIDSASOPTS "(-sysparm $PWD)" -GRIDWAITRESULTS -GRIDSUBMITPGM comline_$USER.sas &
	wait $!
	cat comline_$USER.lst 2> /dev/null
	echo " ";
}