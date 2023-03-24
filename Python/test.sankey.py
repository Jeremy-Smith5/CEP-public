
## the following demonstrates use of tools contained in 'countproc_and_sankey_tools.py'
## Jeremy Smith ~ 2020

import sys
sys.path.append("/home/vhawrjsmithj")

#import sankeytools
from sankeytools import *

printrows = 20

drugevents, fakebounds = mkcpin(
    nppl = 5, 
    outcomes = ['EOD', 'event', 'died'], 
    cohorts = ['grp1', 'grp2'], 
    events = ['drugA', 'drugB', 'drugC', 'drugD', 'drugE'],
    evprobs = [0.3, 0.45, 0.1, 0.25, 0.08]
    )

print ("STEP 1a: fake drug events for CP -- first %s rows" % printrows)

chop(drugevents, nlines = printrows)

print ("STEP 1b: fake patient follow-up boundaries for CP -- first %s rows" % printrows)

chop(fakebounds, nlines = printrows)

drugcp = mkcp(drugevents, fakebounds, headers = True, mkbinary = True, 
	printit = False, mkcsv = False, cpoutname = None, cpoutpath = None
	)

print ("STEP 2: fake CP data -- first %s rows" % printrows)

chop(drugcp, nlines = printrows)

#exit()

smdrugevents = smear(drugcp, left_smear = 10, right_smear = 10)

print ("STEP 3: fake drug events after 'smearing' -- first %s rows" % printrows)

chop(smdrugevents, nlines = printrows)

smdrugcp = mkcp(smdrugevents, fakebounds, headers = True, mkbinary = True, 
	printit = False, mkcsv = False, cpoutname = None, cpoutpath = None
	)

print ("STEP 4: fake CP data after smearing -- first %s rows" % printrows)

chop(smdrugcp, nlines = printrows) 

druglines = mklines(smdrugcp, keep_gaps = False, keep_gapzero = False)

print ("STEP 5: fake drug lines -- first %s rows" % printrows)

chop(druglines, nlines = printrows)
#exit()

drugsrctarg = mkst(
    druglines,
    fakebounds,
    interv = [30, 60, 90, 180],
    minptrows = 1,
    rows_before_interv_end = 'LAST',
    subdiv_outcomes = False,
    expcoh = 'grp1',
    right_truncate = True
    )

import pprint
pp = pprint.PrettyPrinter(indent = 4)

print ("STEP 6: source->target relationships for sankey")

pp.pprint(drugsrctarg)
#dsamp = dict(list(drugsrctarg.items())[1:1])
#pp.pprint(dsamp)
#print (drugsrctarg.keys())

mksankey(drugsrctarg, plot_title = 'STEP 7: Sankey diagram for fake CP')


