## display SAS history file created by updt.sas.hx.py
## and create a simple histogram on the terminal screen
## showing recent SAS use by project for user
# J Smith - Apr 2022

from __future__ import division
import getpass, sys
from math import ceil

runit = False
try:
	apr = sys.argv[1]
	if apr == 'manual': runit = True
except IndexError:
	pass

def sashx(autoprint = False):

	usr = getpass.getuser()

	try:
		f = open('/u/%s/sas_recent_hx.csv' % usr, 'r')
		flines = f.readlines()
		f.close()
		flines = [l.strip() for l in flines[1:]]
	except IOError:
		print '\n...no history file available (/u/%s/sas_recent_hx.csv)\n' % usr
		exit()

	straw = flines[0].split(',')[1]
	flines = flines[1:]

	summary = {}

	for l in flines:
		ll = l.split(',')
		if ll[1] in summary:
			summary.update({ll[1]:summary[ll[1]] + int(ll[3])})
		else:
			summary.update({ll[1]:int(ll[3])})

	sumn = sum(summary.values())

	summary_p = {}

	for k in summary.keys():
		summary_p.update({k:(summary[k] / sumn) * 100})

	maxp = int(ceil(max(summary_p.values())))

	maxp += (maxp % 2 != 0)

	summary_sort = [(k, summary_p[k]) for k in list(summary_p)]
	summary_sort.sort(key = lambda x: x[1], reverse = True)

	out = []
	for i, kv in enumerate(summary_sort):
		out.append('      ' + str(i+1) + ': ' + kv[0] + ' (' + str(round(kv[1],2)) + '%)')

	if autoprint:
		print ' '
		if maxp < 75:
			print '(%)\n'
			for y in range(maxp, -2, -2):
				r = []
				r.append(' ' * (3 - len(str(y))) + str(y) + '   ')
				for kv in summary_sort:
					if round(kv[1]) >= y: r.append('[=]')
					else: r.append('   ')
				print ''.join(r)
			r = []
			r.append('      ')
			r.append('___' * len(summary_sort))
			print ''.join(r)
			r = []
			r.append('      ')
			r.append(' | ' * len(summary_sort))
			print ''.join(r)
			r = []
			r.append('      ')
			r.append(''.join(' ' + str(i+1) + ' ' for i, kv in enumerate(summary_sort)))
			print ''.join(r)
			print ' '
		print 'recent SAS history from %s through today\n' % straw
		for i in out:
			print i
		print ' '
	
	return out

if runit:
	sashx(autoprint = True)
