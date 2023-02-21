
import sys
sys.path.append("/home/vhawrjsmithj")  # <<-- edit to match path where cptools.py is stored 

#import sankeytools
from cptools import *

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

smdrugevents = smear(drugcp, left_smear = 10, right_smear = 10)

print ("STEP 3: fake drug events after 'smearing' -- first %s rows" % printrows)

chop(smdrugevents, nlines = printrows)

smdrugcp = mkcp(smdrugevents, fakebounds, headers = True, mkbinary = True, 
	printit = False, mkcsv = False, cpoutname = None, cpoutpath = None
	)

print ("STEP 4: fake CP data after smearing -- first %s rows" % printrows)

chop(smdrugcp, nlines = printrows) 

