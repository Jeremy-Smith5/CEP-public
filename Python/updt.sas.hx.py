
'''
## PURPOSE: 
	- check whether users wants to track SAS history by presence of ~/keep.SAS.history.txt
	- if so, update the recent history file, and, if needed, archive it

J Smith
29 Apr 2022 
'''

import datetime, os, sys, getpass

sys.path.append('/data/prod/common/WRJ_macros')
import hx_summary

current_prog = sys.argv[1]
homedir = '/u/' + getpass.getuser()

try:
	f = open('%s/keep.SAS.history.txt' % homedir, 'r')
	
except IOError:
	print '\nNOTE: SAS history is NOT being logged...\n'
	exit()

flines = [l.strip() for l in f.readlines()]
f.close()
d = {}
for l in flines:
	k = l.split(':')[0]
	v = l.split(':')[1]
	d.update({k:v})
del flines
if 'n_days' not in d or 'start_on' not in d:
	exit()

ndays = int(d['n_days'])

if ndays <= 0:
	exit()

t = datetime.date.today()
stdate = datetime.datetime.strptime(d['start_on'], '%Y-%m-%d')
straw = d['start_on']

dd = t - stdate.date()

rfile = '%s/sas_recent_hx.csv' % homedir
afile = '%s/sas_archive_hx.csv' % homedir

has_recent = os.path.exists(rfile)

head = 'user,path,file,ntimes'
rdata = {}
if has_recent:
	f = open(rfile, 'r')
	flines = [l.strip() for l in f.readlines()]
	f.close()
	if flines:
		head = flines[0]
		data = flines[2:]  # skipping [1] -- this is just metadata showing the start date for data capture in this file 
		pathloc = head.split(',').index('path')
		fileloc = head.split(',').index('file')
		ntimesloc = head.split(',').index('ntimes')

		for l in data:
			ll = l.split(',')
			pf = ll[pathloc] + '/' + ll[fileloc]  # path/program.sas
			nt = int(ll[ntimesloc])
			rdata.update({pf:nt})
	else:
		os.remove(rfile)

if current_prog in rdata:
	rdata.update({current_prog:rdata[current_prog] + 1})
else:
	rdata.update({current_prog:1})

enddt = stdate + datetime.timedelta(days = ndays - 1)
newstart = stdate + datetime.timedelta(days = ndays)

title = '\n----- WORK PERIOD: %s thru %s -----\n' % (datetime.datetime.strftime(stdate, '%Y-%m-%d'), datetime.datetime.strftime(enddt, '%Y-%m-%d'))

if dd.days >= ndays:
	print '\n...archiving your recent SAS history - here is a summary by project directory:'
	print '...(this file can be found here: %s)...\n' % afile
	shx = hx_summary.sashx(autoprint = True)
	os.system('echo "%s" >> %s' % (title, afile))
	os.system('cat %s >> %s' % (rfile, afile))
	os.system('echo "\n...summary:\n" >> %s' % afile)
	f = open(afile, 'a')
	for ln in shx:
		f.write(ln + '\n')
	f.close()
	os.system('rm -f %s' % rfile)
	os.system('rm -f %s/keep.SAS.history.txt' % homedir)
	f = open('%s/keep.SAS.history.txt' % homedir, 'w')
	f.write('n_days:%s\n' % ndays)
	f.write('start_on:%s' % newstart.date())
	f.close()

f = open(rfile, 'w')
f.write(head + '\n')
if dd.days >= ndays:
	f.write('start date is:,%s,,\n' % newstart.date())
	f.write('%s,%s,%s,1\n' % (getpass.getuser(), os.getcwd(), current_prog[current_prog.rindex('/') + 1:]))
	f.close()
	exit()

f.write('start date is:,%s,,\n' % straw)
for k in rdata.keys():
	f.write('%s,%s,%s,%s\n' % (getpass.getuser(), k[:k.rindex('/')], k[k.rindex('/') + 1:], rdata[k]))

f.close()

exit()

