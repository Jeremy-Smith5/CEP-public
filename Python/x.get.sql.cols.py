
## search the Corporate Data Warehouse VA SQL Server database
## for a specified column and/or table match (based on substring)
## optionally, display the first <n> records of the SQL table
## on screen.
## NOTE this requires the Python module pyodbc, which is available
## in the VINCI development workspaces only (not standard workspaces)

## Jeremy Smith - 2022
## NOTE: this has been aliased as 'sql' 
## NOTE: you will be prompted to enter the schema manually or pick
## from a list of already-visited schemas

import pyodbc
import getpass

import sys, os, subprocess, re
rows, termw=os.popen('stty size','r').read().split()

termw=int(termw)-10

defobs=10
truncwidth=80

def mkvert(nms):
	'''
	nms=[]
	for n in varlist:
		nms.append(n[:n.find(' ')])
	'''
	'''
	lens=[]
	for l in varlist:
		lens.append(l[l.find(' ')+1:])
	'''
	nms = [n + ' ' for n in nms]
	#print (nms)
	#print ('---' * 5)
	vHeader = []
	ml = max([len(n) for n in nms])
	for r in range(ml):
		line=['' for n in nms]
		nnum = 0
		for n in nms:
			if len(n) >= (ml - r): line[nnum]=n[len(n) - ml + r]
			nnum += 1
			
		vHeader.append(line)
	#vHeader.append(lens)	
	return vHeader

usr = getpass.getuser()

drv = 'ODBC Driver 17 for SQL Server'
#svr = 'vhacdwrb03.vha.med.va.gov'
#db = 'CDWWork'
#sch = 'Dim'
#tbl = 'Country'

try:
	f = open('/home/%s/dblist.txt' % usr, 'r')
	flines = f.readlines()
	f.close()
	flines = [l.strip() for l in flines]
except IOError:
	flines = []

enternum = ''
oknums = []
msg = '-' * 30 + ' SQL search -- type q to quit at any prompt ' + '-' * 30
print (msg)

if flines:
	flines = [l for l in flines if l != '']
	print ('\n...list of saved databases:\n')
	enternum = ' or enter number from list above'
	for i, l in enumerate(flines):
		print ('%s) %s' % (str(i + 1), l))
		oknums.append(i + 1)
	print ('\n')

while True:
	dbisnum = False
	isORD = False
	db = input('...which project (enter a name%s:)? ' % (enternum))
	db = db.lower()
	if db == '' or db[0].lower() in ('q', 'e'):
		exit()
	if db.startswith('ord_'):
		db=db[4:]
		isORD = True
	try:
		db = int(db)
		if db not in oknums:
			continue
		db = flines[db - 1]
		dbisnum = True
	except ValueError:
		if isORD:
			db = 'ORD_' + db
	print ('\n...you entered: %s' % db)
	if dbisnum:
		break
	iscorrect = input('...is that right [y/n]? ')
	if iscorrect == '':
		iscorrect = 'y'
	if iscorrect[0].lower() == 'y':
		break
	elif iscorrect == '' or iscorrect[0].lower() in ('q', 'e'):
		exit()

svrOK = False

try:
	f = open('/home/%s/db_svrs.txt' % usr, 'r')
	flines = f.readlines()
	f.close()
	svr = [l.strip() for l in flines if db.lower() in l.lower()]
	if svr:
		svr = svr[0].split(':')[1]
		print ('\n...OK, found %s on server %s' % (db, svr))
		svrOK = True
except IOError:
	pass

if not svrOK:
	for s in ['rb02', 'rb03']:
		svr = 'vhacdw%s.vha.med.va.gov' % s
		conn = pyodbc.connect('Driver={%s};Server=%s;Database=%s;Trusted_Connection=yes;' % (drv, svr, 'CDWWork'))

		query = '''
			select count(name) as nrecs
			from sys.databases where 
			name like '%s'
			''' % db

		cursor = conn.cursor()
		cursor.execute(query)
		rowlist = cursor.fetchall()
		conn.close()
		rowlist = [list(row) for row in rowlist]
		nrows = int(rowlist[0][0])
		if nrows:
			svrOK = True
			f = open('/home/%s/db_svrs.txt' % usr, 'a')
			f.write('\n%s:%s' % (db, svr))
			f.close()
			break

if svrOK:
	print ('\n...found %s on server %s' % (db, svr))
	if not dbisnum:
		f = open('/home/%s/dblist.txt' % usr, 'a')
		f.write('\n'+db)
		f.close()

else:
	print ('\n...sorry, %s was not found on servers RB02 or RB03!' % (db))
	exit()


conn = pyodbc.connect('Driver={%s};Server=%s;Database=%s;Trusted_Connection=yes;' % (drv, svr, db))

while True:
	has_col = True
	has_dom = True

	colsub = input('\n...enter a column name substring to search (blank for all): ')
	if colsub == '':
		has_col = False
	elif colsub.lower() in ('q', 'e'):
		break
	domsub = input('\n...enter a domain/table substring to search (blank for all): ')
	if domsub == '':
		has_dom = False
	elif domsub[0].lower() in ('q', 'e'):
		break
	if not has_col and not has_dom:
		print ('\n...sorry, you must enter a search string for a column, a domain/table or both (or type q to exit)!')
		continue

	tbls_only = False
	if not has_col:
		colsub = '%'
		chkTO = input('Do you want to show all columns (C) or just unique tables (T) [C/T]? ')
		if chkTO == '':
			chkTO = 't'
		elif chkTO[0].lower() in ('q', 'e'):
			exit()
		elif chkTO[0].lower() not in ('c', 't'):
			chkTO = 't'
		if chkTO[0].lower() == 't':
			tbls_only = True
	elif '%' not in colsub:
		if colsub != colsub.upper():
			colsub = '%' + colsub + '%'

	if not has_dom:
		domsub = '%'
	elif '%' not in domsub:
		if domsub != domsub.upper():
			domsub = '%' + domsub + '%'

	if tbls_only:
		query = '''
			select distinct table_catalog as DB, table_schema as sch, table_name as tbl
			from [%s].[INFORMATION_SCHEMA].[COLUMNS] where 
			(table_schema like '%s' or table_name like '%s') 
			order by table_catalog, table_schema, table_name
			''' % (db, domsub, domsub)

	else:
		query = '''
			select table_catalog as DB, table_schema as sch, table_name as tbl, ordinal_position, column_name as col, data_type as type
			from [%s].[INFORMATION_SCHEMA].[COLUMNS] where 
			column_name like '%s' and 
			(table_schema like '%s' or table_name like '%s')
			order by table_catalog, table_schema, table_name, column_name
			''' % (db, colsub, domsub, domsub)

	cursor = conn.cursor()

	cursor.execute(query)

	rowlist = cursor.fetchall()

	rowlist = [list(row) for row in rowlist]

	if not rowlist:
		print ('\n...NO RECORDS FOUND for search: DB: %s ... domain/table: %s ... column: %s\n' % (db, domsub, colsub))
		continue

	rowselect = []
	okRows = []

	if tbls_only:

		while True:

			print ('\n...SEARCH CRITERIA: DB: %s ... domain/table: %s  (table list only)\n' % (db, domsub))
			for i, row in enumerate(rowlist):
				print (i + 1, row)

			printit = input('\nEnter a row number from above to print the first 20 records [BLANK for none]: ')
			if not printit:
				break
			if printit == 'q':
				exit()
			rawvlist = []
			wst = ''
			try:
				printit = int(printit)
			except ValueError:
				rawvlist = printit[printit.find(' ') + 1:]
				rawvlist = re.sub(' +', ' ', rawvlist)
				rawvlist = rawvlist.split(' ')
				rawvlist = [v.lower() for v in rawvlist]
				if 'w' in rawvlist:
					rawvlist.remove('w')
					wst = input('WHERE [blank for none]: ')
					if wst:
						wst = 'WHERE ' + wst
				printit = int(printit[:printit.find(' ')])
			'''
			except (AttributeError, ValueError):
				printit = int(printit)
				print ('this INT step is happening')
			'''
			if printit < 1 or printit > len(rowlist):
				continue
			
			wilds = []
			vlist = []
			if rawvlist:
				vorder = [(i, v) for i, v in enumerate(rawvlist)]
				wilds = [v.lower() for v in rawvlist if ':' in v]
				vlist = [v.lower() for v in rawvlist if ':' not in v]

				if wilds:
					wildtype = {}
					wi = 0
					for w in wilds:
						if w[0] == ':' and w[len(w) - 1] == ':': t = 1 	# 1=both
						elif w[0] == ':': t = 2			# 2=start only
						else: t = 3 				# 3=end only
						wildtype.update({w.replace(':', '') : t})
						wilds[wi] = w.replace(':', '')
						wi += 1

			tbldb = rowlist[printit - 1][0]
			tblsc = rowlist[printit - 1][1]
			tblnm = rowlist[printit - 1][2]

			srcview = '[%s].[%s].[%s]' % (tbldb, tblsc, tblnm)

			query = '''
				select top 10 * from %s %s
				''' % (srcview, wst)

			cursor = conn.cursor()
			cursor.execute(query)
			top10 = cursor.fetchall()

			top10 = [list(row) for row in top10]
			
			hdrqry = '''
				select column_name
				from [%s].[INFORMATION_SCHEMA].[COLUMNS] where 
				(table_schema = '%s' and table_name = '%s') 
				order by ordinal_position
				''' % (tbldb, tblsc, tblnm)

			#cursor = conn.cursor()
			cursor.execute(hdrqry)
			top10header = cursor.fetchall()
			#conn.close()

			top10header = [row[0] for row in top10header]
			
			f = []
			h = []

			if vlist or wilds:
				r = 0
				for row in top10:
					r += 1
					newrow = []
					for name, v in zip(top10header, row):
						if name.lower() in vlist: 
							newrow.append(v)
							if name not in h: h.append(name)
						elif wilds:
							for w in wilds:
								if wildtype.get(w) == 1 and w in name.lower(): 
									newrow.append(v)
									if name not in h: h.append(name)
								elif wildtype.get(w) == 2 and name.lower().endswith(w): 
									newrow.append(v)
									if name not in h: h.append(name)
								elif wildtype.get(w) == 3 and name.lower().startswith(w): 
									newrow.append(v)
									if name not in h: h.append(name)
					f.append(newrow)
			else: 
				h = top10header[:]
				f = top10[:]

			H = mkvert(h)

			r = 0
			anytrunc = 0
			for row in f:
				r += 1
				for v in range(len(row)):
					pass
					'''
					if r == 1:
						#row[v]=h[v]
						row[v]='---'
					else:
					'''
					'''
					if '$' not in h[v]:
						if row[v] == None: row[v] = '.'
						else:
							try:
								if int(row[v]) == row[v]: row[v] = int(row[v])
								else: row[v] = round(row[v],2)
							except (TypeError, OverflowError) as e:
								pass
					else:
					'''
					'''
					if len(row[v]) > truncwidth:
						anytrunc = 1
						row[v] = row[v][:truncwidth]
					'''

			headw = [max(map(len, map(str, col))) for col in zip(*H)]
			colw0 = [max(map(len, map(str, col))) for col in zip(*f)]

			colw = [max(h,v) for h, v in zip(headw, colw0)]

			if max(colw) >= termw:
				print ("At least 1 variable is too wide to fit in this terminal!")
				print ("Maximize the terminal and re-run this script.")
				exit()

			if anytrunc:
				print ("\nAt least one variable was truncated d/t length > %s!\n" % truncwidth)
			print ("\n--- first %s obs of %s --- (%s cols)\n" % (10, tblnm, '{:,}'.format(len(h))))

			lbound = 0
			v = 0
			while v < len(f[0]):
				chunkwidth = 0
				while v < len(f[0]):
					if (chunkwidth + colw[v] + ((v - lbound) * 2)) <= termw:
						chunkwidth += colw[v]
						if v < len(f[0]): v += 1
					else: break
					
				for row in H:
					print ("  ".join((str(val).ljust(width) for val, width in zip(row[lbound:v], colw[lbound:v]))))
				
				for row in f:
					print ("  ".join((str(val).ljust(width) for val, width in zip(row[lbound:v], colw[lbound:v]))))

				print ("\n")

				lbound = v

			nexttbl = input("\nPress ENTER to continue...")

	else:
		print ('\n...SEARCH CRITERIA: DB: %s ... domain/table: %s ... column: %s\n' % (db, domsub, colsub))

		for i, row in enumerate(rowlist):
			print ('%s) %s' % (str(i + 1), row))
			if row[0].startswith('ORD_') or row[1] == 'Dim':
				okRows.append(i)
				rowselect.append('select top 20 %s from [%s].[%s].[%s]' % (row[4], row[0], row[1], row[2]))
				#rowselect.append('select top 20 * from [%s].[%s].[%s]' % (row[0], row[1], row[2]))
			else:
				rowselect.append('--not accessible--')
		while True:
			rowprint = input('\n...enter a row number from above (and optional WHERE clause) to print top 20 records (blank for none): ')
			if rowprint in ('q', 'e'):
				conn.close()
				exit()
			elif rowprint != '':
				ws = ''
				rowprint = rowprint.lower()
				if rowprint.endswith(' nn'):
					#rowprint = rowprint.replace(' nn',' where %s is not null' % row[3])i
					rowprint = rowprint.replace(' nn',' where %s is not null' % rowlist[int(rowprint[:rowprint.find(' ')])-1][4])
				if 'where' in rowprint:
					ws = rowprint[rowprint.find('where'):]
					rowprint = rowprint[:rowprint.find(' ')]
				try:
					rowprint = int(rowprint)
					if rowprint-1 in okRows:
						cursor = conn.cursor()
						qry = rowselect[rowprint - 1] + " " + ws
						cursor.execute(qry)
						rowlist = cursor.fetchall()
						rowlist = [list(row) for row in rowlist]
						print ('\n...QUERY: %s %s\n' % (rowselect[rowprint - 1], ws))
						if rowlist:
							for row in rowlist:
								print (row)
						else:
							print ('\n...sorry - no records found for this query')
					else:
						print ('\n...sorry, record %s was not found in the list above!\n' % rowprint)
				except ValueError:
					pass
			else:
				break

conn.close()

