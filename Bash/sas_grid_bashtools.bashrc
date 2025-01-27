
## the following are 3 functions for streamlining interacting with SASGSUB folders/files
## for SAS Grid environments

# sasdiff: compare the program identified by argument 1 with its SASGSUB 
# counterpart identified by the number of iterations (arg 2) back in the time-sorted
# list of SASGSUB folders containing the program of interest, with 0 being the
# most recent run of this program.  If argument 2 is not specified, 0 will be 
# assumed, that is, the current version of the program will be compared with 
# its state as of the most recent run.  To list all associated SASGSUB directories
# first, enter ? as arg2 (the most recent will be last in this list). NOTE: a simple 
# way to compare the current version to the *earliest* version without knowing the 
# number of associated SASGSUB folders is to specify arg2 as -1 (-2 for 2nd earliest, etc.).
function sasdiff {
	lsout=()
	if ! [[ $1 =~ \.sas$ ]]; then
		echo; echo "USAGE: sasdiff <full program name>.sas [lookback # (default: 0) or ? for list]"; echo;
		return
	fi
	sasname=$1
	sasname=${sasname::-4}
	pattern="SASGSUB-[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}\.[0-9]{2}\.[0-9]{2}\.[0-9]{3}_$sasname\/"
	while IFS= read -r line; do
		if [[ $line =~ $pattern ]]; then
			sgdir=SASGSUB${line#*SASGSUB}
			lsout+=( "$sgdir" )
		fi
	done < <(ls -altd */ | grep SASGSUB)
	lscount=${#lsout[@]}
	if [[ $lscount == 0 ]]; then
		echo; echo "...no SASGSUB records found for $1"; echo;
		return
	fi
	if [ "$2" == "?" ]; then
		sgnum=$lscount
		for (( i=$lscount-1; i>=0; i-- )); do
			sgnum=$((sgnum - 1))
			echo "$sgnum: ${lsout[i]}"
		done
		echo; echo "$lscount SASGSUB records were found for the SAS program $1"; echo;
		read -p "Enter the directory number for comparison (0 for most recent or [Enter] to quit): " lbnum
		if ! [[ $lbnum =~ ^[0-9]+$ ]]; then
			echo; echo "-- NO COMPARISON DONE --"; echo;
			return
		fi
	else
		lbnum=0
		if [[ $2 =~ ^[0-9]+$ ]]; then
			lbnum=$2
		fi
	fi
	if [[ $lbnum > $((lscount - 1)) ]]; then
		echo "USAGE: for the program $1, you must specify a lookback number between 0 and $((lscount - 1))"; echo;
		return
	fi
	lbdir=${lsout[$lbnum]}
	echo; echo "...differences in $1 for CWD vs. $lbdir:"; echo;
	diff $1 $lbdir$1
	echo;
}

## list SASGSUB folders in CWD, optionally subsetting to those whose names match a keyword (not case-sens)
## ex:  1) lsg  ... 2) lsg state_rates
function lsg {
	lsout=()
	if [ "$1" != "" ] && ! [[ $1 =~ ^[0-9]+$ ]]; then
		while IFS= read -r line; do
			if [[ $(echo "$line" | grep -i "$1") != "" ]]; then
				lsout+=( "$line" )
			fi
		done < <(ls -alrtd */ | grep GSUB)
	else
		while IFS= read -r line; do
			lsout+=( "$line" )
		done < <(ls -alrtd */ | grep GSUB)
	fi
	nrecs=${#lsout[@]}
	if [[ $1 =~ ^[0-9]+$ ]]; then
		nrecs=$1
	elif [[ $2 =~ ^[0-9]+$ ]]; then
		nrecs=$2
	fi
	for line in "${lsout[@]: -$nrecs}"; do
		echo "$line"
	done
	echo; echo "TOTAL RECORDS: $nrecs"; echo;
}

function gsclean_usage () {
	echo " "
	echo "FUNCTION gsclean():"
	echo " "
	echo "PURPOSE: archive or delete specific GSUB folders within a stated time range or except for the last <N>"
	echo "...valid flags are: "
	echo "------ 'p' REQUIRED - <program name search string> -- default ALL -- use separate -p args to specify multiple strings"
	echo "------ 'n' <# to keep> -- default 5 -- NOTE: this is overridden if the -r (date range) argument is specified"
	echo "------ 'r' prompt for date range to move / delete (inclusive on both sides) -- default FALSE"
	echo "		 ... NOTE: if -r is specified, START DATE prompt will default to 2000-01-01 and END DATE prompt will default to TODAY"
	echo "------ 'm' move folders/contents to ./GSUB instead of deleting -- default FALSE"
	echo "------ 'v' verbose (complete) details - list all folders to be affected instead of a summary -- default FALSE"
	echo "------ 'a' all directories - allows move / deletion of all SASGSUB directories instead of just those associated with $USER"
	echo "------ 'f' 'forces' move / deletion WITHOUT seeking confirmation for each program - default FALSE"
	echo " "
	echo "EXAMPLE CALLS:"
	echo "gsclean -p map -n 10 -v    -- delete all GSUB folders from programs containing 'map' in the name except for the 10 most recent"
	echo "gsclean -p map -r -m -a    -- move all GSUB folders containing 'map' in the name within a specified date range including those created by other users"
	echo " "
	echo " "
	}

## GSCLEAN: see gsclean_usage above for documentation / usage
function gsclean() {
	local OPTIND flag progs nkeep force ndays move verbose all_users use_dtrange today today_epoch progs plist action
	local files ntries rngDONE rngstart rngstart_epoch rngend rngend_epoch datesOK glist plist n_this_prog 
	local submsg confirm line gdir gfile prog gdate gdate_epoch days_old inrange startdate enddate ng final_glist ndel dir 
	local files alldirs unqprogs uprog gfile_found doit
	today=$(date +%Y-%m-%d)
	today_epoch=$(date -d "$today" +%s)
	nkeep=5
	ndays=10000
	move=0
	verbose=0
	use_dtrange=0
	all_users=0
	force=0
	while getopts 'p:n:mvraf' flag; do
		case "${flag}" in
			p) progs+=" ${OPTARG}" ;;
			n) nkeep=${OPTARG} ;;
			#d) ndays=${OPTARG} ;;
			m) move=1 ;;
			v) verbose=1 ;;
			r) use_dtrange=1 ;;
			a) all_users=1 ;;
			f) force=1 ;;
			*) gsclean_usage
			   return ;;
		esac
	done
	shift $((OPTIND - 1))

	progs="${progs:1}"

	# NOTE: this 'ALL' option is not working - tried using IFS='\n' in next step, but result was truncated to first 40 char of list
	if [[ "$progs" == "ALL" ]]; then
		progs="$(ls -1 *.sas)"
	fi

	IFS=$'\n' read -d '' -r -a plist <<< "${progs}"
	
	if [ ${#plist[@]} == 0 ]; then
		gsclean_usage
		return
	fi
	
	action='DELETED'
	if [[ $move == 1 ]]; then
		action='moved to ./GSUB'
	fi
	submsg=""
	if [[ $use_dtrange == 1 ]]; then

		declare -i ntries ntries=0
		declare -i rngDONE rngDONE=0
		while [[ $rngDONE == 0 ]]; do
			echo " "
			read -p "Enter a START DATE for the range you want to be $action (YYYY-MM-DD) [default: 2000-01-01]: " rngstart
			if [[ "$rngstart" == "" ]]; then 
				rngstart="2000-01-01" 
			fi
			read -p "Enter an END DATE for the range you want to be $action (YYYY-MM-DD) [default: TODAY]: " rngend
			if [[ "$rngend" == "" ]]; then 
				rngend="$today" 
			fi

			echo " "
			read -p "Is [$rngstart through $rngend] the correct date range to be $action? ['n' / <Enter> for 'y' / 'q' to quit]: " datesOK
			echo " "

			if [[ "$datesOK" == "q" ]] || [[ $ntries == 3 ]]; then
				return
			else
				if [[ "$datesOK" != "" ]] && [[ "$datesOK" != "y" ]] && [[ "$datesOK" != "Y" ]]; then
					((ntries++))
					if [[ $ntries -ge 3 ]]; then
						return
					fi
				else 
					rngDONE=1
					rngstart_epoch=$(date -d "$rngstart" +%s)
					rngend_epoch=$(date -d "$rngend" +%s)
				fi
			fi
			submsg=" between $startdate and $enddate"
		done
	else
		echo " "
		echo "...keeping last $nkeep SASGSUB directories for each program"
		echo " "
	fi

	echo " "
	echo "...The following SASGSUB folders and contents will be $action:"
	echo " "
	for prog in "${plist[@]}"; do
		if [[ "${prog:(-4)}" == ".sas" ]]; then
			prog="${prog:0:-4}"
		fi
		IFS='/' read -a alldirs <<< $(ls -ld SASGSUB-????-??-??_??.??.??.???_*/ | grep $prog)
		unqprogs=()
		for line in "${alldirs[@]}"; do
			gdir="SASGSUB-${line#*'SASGSUB-'}"
			gfile="${gdir:32}"
			gfile_found=0
			for p in "${unqprogs[@]}"; do
				if [[ $p == $gfile ]]; then
					gfile_found=1
					break
				fi
			done
			if [[ $gfile_found == 0 ]]; then
				unqprogs+=( "$gfile" )
			fi
		done
		for uprog in "${unqprogs[@]}"; do
			files=()
			if [[ $all_users == 0 ]]; then
				IFS='/' read -a files <<< $(ls -alrtd SASGSUB-????-??-??_??.??.??.???_${uprog}/ | grep $USER\ $USER)
			else
				IFS='/' read -a files <<< $(ls -alrtd SASGSUB-????-??-??_??.??.??.???_${uprog}/)
			fi
			startdate=""
			declare -i n_this_prog n_this_prog=0
			glist=()
			for line in "${files[@]}"; do
				gdir="SASGSUB-${line#*'SASGSUB-'}"
				gdate="${gdir:8:10}"
				gdate_epoch=$(date -d "$gdate" +%s)
				days_old=$((($today_epoch-$gdate_epoch)/86400))
				inrange=0
				if [[ $use_dtrange == 1 ]]; then
					if [[ $gdate_epoch -ge $rngstart_epoch ]] && [[ $gdate_epoch -le $rngend_epoch ]]; then
						inrange=1
					fi
				elif [[ $days_old > $ndays ]]; then
					inrange=1
				fi
				if [[ $inrange == 1 ]]; then
					if [[ $startdate == "" ]]; then
						startdate=$gdate
					fi
					enddate=$gdate
					glist+=( "$gdir" )
					((n_this_prog++))			
				fi
			done
			ng=${#glist[@]}
			if [[ $ng == 0 ]]; then
				continue
			fi
			final_glist=()
			if [[ $nkeep -lt $ng ]] && [[ $use_dtrange == 0 ]]; then
				ndel=$((ng-nkeep))
				for dir in "${glist[@]:0:ndel}"; do
					final_glist+=( "$dir" )
				done
				n_this_prog=${#final_glist[@]}
			else
				final_glist=("${glist[@]}")
			fi
			if [[ $n_this_prog > 0 ]]; then
				echo ' '
				echo "------ $uprog directories$submsg (TOTAL: $n_this_prog)"
				if [[ $verbose == 1 ]]; then
					echo ' '
					for dir in "${final_glist[@]}"; do
						echo "$dir"
					done
				fi
				echo ' '
				doit=1
				if [[ $force == 0 ]]; then
					doit=0
					read -p "$uprog: type 'yes', 'q' to quit or [Enter] to skip: " confirm
					if [[ "$confirm" == "q" ]]; then
						return
					elif [[ "$confirm" == "yes" ]]; then
						doit=1
						echo "...$uprog directories will be $action"
					else
						echo "...$uprog directories skipped"
					fi
				fi
				if [[ $doit == 1 ]]; then
					if [[ $move == 1 ]]; then
						mkdir -pv GSUB
						for dir in "${final_glist[@]}"; do
							mv -v "$dir" ./GSUB
						done
					else
						for dir in "${final_glist[@]}"; do
							rm -f $dir/*
							rmdir $dir
						done
					fi
				fi
			fi
		done
	done
}