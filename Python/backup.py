
'''
Back up SAS, Python and shell scripts (.sas, *.py, *.sh) from SAS Grid 
directories specified in 'backup_dirs.txt' to their respective P: drive
locations, keeping the 3 most recent distinct copies for each. Parent 
directories and all subdirectories will be searched for scripts to 
back up unless " (only)" is specified as part of the label for the directory
in the backup_dirs.txt file.

If backing up from a Grid location for which no corresponding P: drive
folder exists, backup to the location specified as 'alternateORD' below.

In order for this script to work, you need to create a text file containing
a list of all parent directories to be backed up and specify that text files
name / path in the 'try:' block below that is underneath the comment 'get list
of active ORD parent directories'.

This text file containing parent directories should look like this:

/data/prod/common/WRJ_macros -- common drive macros
/data/dart/2021/ord_davies_202110040d/Programs -- disrupted care
/data/dart/2021/ord_korves_202109021d/Programs -- AMI2
/data/dart/2022/ord_korves_202209016d/Programs -- Flu Dur


Run this program from the Bash command line -- this assumes the presence
of Python 2.x 

alias bu='python /data/prod/common/WRJ_macros/backups/utils/backup.py'

Call:
bu

J Smith
2021
'''

import time
import os, sys, getpass, shutil, filecmp

#alternateORD = 'ord_youngxu_201703014d' # to be used as P: drive location for WRJ_macros backups only!
alternateORD = 'ord_korves_202209016d'
completealt = '/data/dart/%s/%s' % (alternateORD.split('_')[2][:4], alternateORD)

cdate = time.ctime() # <<-- needs formatting! 
ctime = str(int(time.time()))

#alerts = '/data/prod/common/WRJ_macros/backups/utils/alerts.txt'
alerts = '~/pdrive_backup_alerts.txt'

def mkalert(alert, alertfile = alerts):
	f = open(alertfile, 'a')
	f.write('%s: ' + alert % cdate)
	print('ALERT: ' + alert)
	f.close()  

# get list of active ORD parent directories
try:
	f = open('/data/prod/common/WRJ_macros/backups/utils/backup_dirs.txt', 'r')
except IOError:
	mkalert('%s: no directory list found -- no backups done!' % cdate)
	exit()
	
dirlist = [d.rstrip('\n') for d in f.readlines() if d != '' and d != '\n' and not d.startswith('--')]
f.close()

print ('\nThe following parent directories (with children, unless "(only)" is specified) will be backed up to their respective P: drive locations:\n')
for d in dirlist:
	comment = ''
	if d.find('WRJ_macros') > -1:
		comment = '-- will be backed up to: ' + alternateORD
	print ('-- %s %s' % (d, comment))

mkbkups = raw_input('\n-- Do you want to run the backups utility? [type "y" to run, or <Enter> to cancel]: ')
if not mkbkups:
	print ('-- backup cancelled')
	exit()
if not mkbkups.upper()[0] == 'Y':
	print ('-- backup cancelled')
	exit()

# loop thru this list:
for rootdir in dirlist:
	parentonly = False
	comment = ''
	if rootdir.find('--') > -1:
		rootsplit = rootdir.split('--')
		comment = rootsplit[1].strip()
		if comment.upper().find('(ONLY)') > -1:
			parentonly = True
		rootdir = rootsplit[0].strip()
	if rootdir.endswith('/'):
		rootdir = rootdir[:-1]
	if not rootdir.startswith('/data'):
		mkalert('%s is not a valid directory for backups - skipping!' % rootdir)
		continue
	# find ORD portion for use later
	usealt = False
	if rootdir.find('/ord_') == -1:
		if rootdir.find('WRJ_macros') > -1:
			usealt = True
		else:
			mkalert('The root directory %s is not a valid ORD directory - skipping!' % rootdir)
			continue
	print ('\n-- checking directory tree starting from: %s -- %s\n' % (rootdir, comment))
	if not usealt:
		ordcomplete = rootdir[rootdir.find('/ord_') + 1:] # e.g., if rootdir is '/data/dart/2019/ord.../Programs/TRD, then ordcomplete is ord.../Programs/TRD
		orddir = ordcomplete.split('/')[0]  # just the ord segment
		postord = ''
		if ordcomplete.find('/') != -1:
			postord = ordcomplete[ordcomplete.find('/') + 1:]  # e.g., from example above, postord is /Programs/TRD
	else:
		ordcomplete = alternateORD
		orddir = alternateORD
		postord = 'WRJ_macros'

	pdrroot = '/cifs3/vhacdwfpcfs02/%s/projects/%s' % (getpass.getuser(), orddir)
	# make sure the above is a writable directory
	try:
		f = open('%s/_testfile_' % pdrroot, 'w')
		f.close()
	except IOError:
		mkalert('The P: drive location %s does not exist - skipping!' % pdrroot)
		continue
	pdr = pdrroot + '/backups/%s' % postord  # from example above, pdr is /cifs3/.../projects/ord.../backups/Programs/TRD
	if pdr.endswith('/'):
		pdr = pdr[:-1]
	try:
		os.makedirs(pdr)
	except OSError:
		pass

	anyerr = False
	total_files_checked = 0
	total_backups_created = 0
	startroot = ''
	for root, dirs, files in os.walk(rootdir, topdown = True):
		dirs[:] = [d for d in dirs if not d.startswith('SASGSUB')\
		and not d.upper() == 'GSUB' and not d.upper().startswith('ARCHIVE')\
		and not d.upper().startswith('OLD') and not d.upper().startswith('JUNK')\
		and not d.upper().startswith('FROM_')\
		and not d.upper().startswith('GARBAGE') and not d.upper().startswith('BAD_')\
		and not d.upper().startswith('TRASH')\
		and not d.upper() == 'DATA' and not d.upper().startswith('COPY')\
		and not d.upper().startswith('COPIES')]

		files[:] = [f for f in files\
		if (f.endswith('.sas') or f.endswith('.py') or f.endswith('.sh') or f.endswith('bashrc'))\
		and not f.upper().startswith('OLD_')\
		and not f.upper().startswith('OLD.')\
		and not f.upper().startswith('COMLINE') and not f.upper().startswith('SQLDROP')]

		for gf in files:
			if parentonly and root != rootdir:
				continue
			if startroot != root:
				print('-- -- scanning %s' % root)
				startroot = root
			rd = root
			p = root[len(rootdir) + 1:]
			if p.startswith('/'):
				p = p[1:]
			rdp = pdrroot + '/backups/' + postord + '/' + p
			try:
				os.makedirs(rdp)
			except OSError:
				pass
			proglist = [f for f in os.listdir(rdp) if f.endswith('.sas')]
			total_files_checked += 1
			# check whether gf exists in rdp
			slist = [pf for pf in proglist if pf[pf.find('_') + 1:] == gf]
			outf = rdp + '/' + ctime + '_' + gf
			if slist:
				slist[:] = [x for i, x in sorted(zip([int(pf[:pf.find('_')]) for pf in slist], [pf for pf in slist]))]
				newest = slist[-1]
				# check whether most recent backup is different from the current file
				issame = filecmp.cmp(rd + '/' + gf, rdp + '/' + newest, shallow = True)
				if issame:
					continue
			# remove the oldest from the backup folder if there are already 3 copies present
			while len(slist) > 2:
				old = slist.pop(0)
				try:
					os.remove(rdp + '/' + old)
				except IOError:
					if not anyerr:
						mkalert('Problems removing old backups in %s!' % rdp)
					anyerr = True
					break
			
			shutil.copyfile(rd + '/' + gf, outf)
			total_backups_created += 1

	print ('\n-- -- TOTAL FILES CHECKED: %s' % str(total_files_checked))	
	print ('-- -- TOTAL BACKUPS CREATED: %s' % str(total_backups_created))

print ('\n-------------------------------------------- DONE --------------------------------------------\n')
