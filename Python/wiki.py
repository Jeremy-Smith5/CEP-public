## read from / write to a central 'wiki' file allowing the group 
## to capture and share knowledge - specifically, this is for 
## making note of SAS programs or directories where specific 
## types of analyses or methods have been performed
## see 'SYNTAX' below for instructions

## Jeremy Smith  ~ 2018
## to use, call from another script as follows:
'''
import sys

# append path to this Python script
sys.path.append('/data/prod/common/WRJ_macros')

from wiki import wiki_write

args = sys.argv[1:]

# note: the wiki file has an sh extension only so that it is seen by the backups program
# it is really just a text file!
wiki_write(<path to wiki file>, (a for a in args))
'''

def wiki_write(wfile, *argv):
	'''
	try:
		f = open(wfile, 'a')
		f.close()
	except IOError:
		print '\nYou do not have write access to %s!\n' % wfile
		return
	'''
	arglist = argv[0]
	arglist = [a for a in arglist]
	try:
		chk = arglist[0]
	except IndexError:
		print '''
\nSYNTAX:\n
    wiki -- (without arguments) -- show this syntax
    wiki %  -- show full list of current search terms in the wiki
    wiki %<search term> -- e.g., %viral -- show current search terms containing 'viral' (not case sens.)
    wiki <search_term 1> <search_term 2> ... <search_term n> -- terms to ADD to the wiki relating to this DIRECTORY (do not use %)
    wiki <SAS prog> <search_term 1> <search_term 2> ... <search_term n> -- terms to add to the wiki relating to <SAS prog> (incl. .sas extension!)

    NOTES: 
	- when SEARCHING the wiki, add a % to the beginning of the search term.  The % is meant to substitute for the Twitter-like hashtag, 
	  using % instead because # would have to be escaped.  % is NOT a wildcard.  You can only search one term at a time.
	  Example: wiki %antiviral 

	- to see all existing search terms in the wiki, just use a % by itself, i.e., wiki %

	- the search term is not case-sensitive and will be compared as a substring of all the existing terms in the wiki.

	- when ADDING terms to the wiki, you can specify a program name in the current directory and list one or more terms you
	  want to associate with it, e.g., wiki pullMedsVA.sas opRx opmeds rxOutpat antivirals

	- if you just want to associate the current working directory with a list of search terms, just list the search terms 
	  immediately after 'wiki', e.g., wiki opRx outpatientmeds opmeds antibiotics

	- do not use spaces or any characters other than letters, numbers and underscores (same as SAS variables)

	- if your term already exists in the wiki, your program or directory will just be added to the existing term's references

	- it may be helpful to search the list for the term you're planning to use to help in aligning it with existing terminology, though
	  feel free to add alternate terms as shown above for outpatient meds
		'''
		return

	wd = {}
	try:
		f = open(wfile, 'r')
		flines = f.readlines()
		f.close()
		if len(flines) == 0:
			pass
		else:
			for line in flines:
				if line == '\n':
					continue
				kv = [i.strip() for i in line.split(',')]
				wd.update({kv[0]:kv[1:]})
	except IOError:
		pass
	
	if arglist[0].startswith('%'):
		
		def write_aliases(aliases):
			if aliases:
				import getpass
				with open('/u/%s/.wiki_aliases' % getpass.getuser(), 'w') as f:
					for k in aliases.keys():
						f.write('w%s=%s\n' % (str(aliases[k][0]), k))
						f.write('alias wp%s="pushd %s"\n' % (str(aliases[k][0]), aliases[k][1]))
						if k.endswith('.sas'):
							f.write('alias wv%s="vim %s"\n' % (str(aliases[k][0]), k))
							f.write('alias wl%s="less %s"\n' % (str(aliases[k][0]), k))
				#print '\n--------------------------------------'
				#print '-- Type ww to source the above aliases'
				#print '--------------------------------------\n'
		if len(wd) == 0:	
			print '\nSorry, the Wiki is currently empty!\n'
			return

		disp = {}
		st = None
		if arglist[0] != '%':
			st = str(arglist[0][1:]).strip()

		nkeys = 0
		for i, k in enumerate(wd.keys()):
			if st:
				if st.lower() in k.lower():
					nkeys += 1
					keyval = k
					disp.update({nkeys:k})
			else:
				disp.update({i + 1:k})
		if st and not nkeys:
			print '\n -- Sorry, no items match the search term %s -- \n' % str(st)
			return
		aliases = {}
		nal = 0
		if nkeys == 1:
			# special case -- search term was provided and had exactly one match
			# just display the results
			print '\n -- Items found matching %s (Wiki: %s) -- \n' % (str(st), keyval)
			for m in wd[disp[1]]:
				if m not in aliases.keys():
					isProg = m.endswith('.sas')
					nal += 1
					pth = m
					if m.endswith('.sas'):
						pth = m[0:m.rfind('/')]
					aliases.update({m:(nal, pth)})

				if m.endswith('.sas'):	
					print '>> %s -- ($w%s) ... pushd: wp%s ... vim: wv%s ... less: wl%s' % (m, nal, nal, nal, nal)
				else:
					print '>> %s -- ($w%s) ... pushd: wp%s' % (m, nal, nal)

			print '\n'
			write_aliases(aliases)
			return
		
		print '\n -- Current search terms in the Wiki: -- \n'
		for k in disp.keys():
			print '%s: %s' % (str(k), disp[k])

		print '\n'
		items = raw_input('\nEnter the item number/s you want to display (sep w spaces) (type ALL to show all or <Enter> to quit): ')
		if items.strip() == '':
			return
		if items.strip().lower() == 'all':
			items = [k for k in disp.keys()]
		else:
			items = [int(i) for i in items.split(' ')]
		for i in items:
			try:
				print '\n -- %s -- \n' % disp[i]
				matches = wd[disp[i]]
				for m in matches:
					if m not in aliases.keys():
						isProg = m.endswith('.sas')
						nal += 1
						pth = m
						if m.endswith('.sas'):
							pth = m[0:m.rfind('/')]
						aliases.update({m:(nal, pth)})

					if m.endswith('.sas'):	
						print '>> %s -- ($w%s) ... pushd: wp%s ... vim: wv%s ... less: wl%s' % (m, nal, nal, nal, nal)
					else:
						print '>> %s -- ($w%s) ... pushd: wp%s' % (m, nal, nal)

			except KeyError:
				print '\n...%s was not in the list displayed above.\n' % str(i)
		write_aliases(aliases)
		print '\n'
		return

	else:
		print '\n-------------------------------------------------------------------------------------------'
		confirm_add = raw_input('Please confirm you want to ** ADD TERMS ** to the wiki! (type n for no or <Enter> for yes): ')
		if confirm_add != '' and not confirm_add.lower().startswith('y'):
			return
		try:
			f = open(wfile, 'w')
			# user is adding terms to the wiki
			import os
			refloc = os.getcwd()
			startpos = 0
			if arglist[0].lower().endswith('.sas'):
				refloc = refloc + '/' + arglist[0]
				startpos = 1
			try:
				terms = arglist[startpos:]
			except IndexError:
				print '\n -- You must enter a list of terms you want associated with %s! -- ' % refloc
				print ' -- Type wiki for syntax -- \n'
				return
			nupdates = 0
			for t in terms:
				# check whether t already in wiki - if so, add this refloc
				tfound = False
				for k in wd.keys():
					if t.lower() == k.lower():
						tfound = True
						if refloc not in wd[k]:
							print '\n...added to existing wiki term %s' % k
							nupdates += 1
							wd[k].append(refloc)
						break
				if not tfound:
					print '\n...added NEW wiki term %s' % t
					nupdates += 1
					wd.update({t:refloc})
		
			for k in wd.keys():
				entry = [k]
				wdk = wd[k]
				if type(wdk) == list:
					for e in wdk:
						entry.append(e)
				else:
					entry.append(wdk)
				f.write(','.join([str(i) for i in entry]))
				f.write('\n')

			f.close()
			print '\n...%s terms were added / updated\n' % str(nupdates)
			return
	
		except IOError:
			print '\n -- Sorry, you do not have write access to the file %s -- \n' % wfile
			return
