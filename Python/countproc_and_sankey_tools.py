
## this is a set of tools for creating counting process data
## and displaying longitudinal exposure (in bins) for 
## one or more cohorts in that data as a Sankey diagram
## using the Python plotly module
## this is definitely a work-in-progress - choosing
## appropriate time bins and exposure categories in order
## to display a sensible plot is challenging

## Jeremy Smith ~2017
## see sample call in test.sankey.py

# not being used - just converts a sql table to a list of lists
def tbl2pylist(tbl):
    pylist = sqlContext.sql("select * from %s" % tbl)
    pylist = pylist.toPandas()
    pylist = pylist.values.tolist()
    
    return pylist
    
# formats and prints the first <nlines> of a 'dataset' (list of lists)
def chop(data, termw = 80, nlines = 20):
    colw = [max(map(len, map(str, col))) for col in zip(*data)]
    lbound = 0
    v = 0
    termw = termw or 80
    nlines = nlines or 20
    nlines = min(nlines, len(data))
    
    while v < len(data[0]):
        chunkwidth = 0
        while v < len(data[0]):
            if chunkwidth + colw[v] <= termw:
                chunkwidth += colw[v]
                if v < len(data[0]): v += 1
            else:
                break
        for row in data[:nlines + 1]:
            print(' '.join(str(var).ljust(w) for var, w in zip(row[lbound:v], colw[lbound:v])))
        print ("\n")
        lbound = v

# makes a simulated 'dataset' (list of lists) to be used as input for the mkcp() function further down
def mkcpin(
    nppl,
    minfu = 100, 
    outcomes = ['EOD', 'event', 'died'], 
    cohorts = ['grp1', 'grp2'], 
    events = ['drugA', 'drugB', 'drugC', 'drugD', 'drugE'],
    evprobs = [0.3, 0.45, 0.1, 0.25, 0.08]
    ):

    from random import randint
    from random import random    
    cpin = []
    bnds = []
    cpinhead = ['pt', 'event', 'edate', 'days']
    bndshead = ['pt', 'start', 'end', 'outcome', 'cohort']
    cpin.append(cpinhead)
    bnds.append(bndshead)

    supps = [15, 30, 30, 30, 30, 30, 45, 60, 60, 90]
    
    nppl = nppl or 100
    if not evprobs:
        evprobs = [random() for i in events]
    for pt in range(1, nppl + 1):
        # start = randint(1, 500)
        start = 0
        end = randint(start + minfu, start + minfu * 3)
        outcome = outcomes[randint(0, len(outcomes) - 1)]
        cohort = cohorts[randint(0, len(cohorts) - 1)]
        ptbrow = [pt, start, end, outcome, cohort]
        bnds.append(ptbrow)
        ptexps = [e for e, r in zip(events, evprobs) if random() < r]
        if not ptexps: 
            continue
        edate = randint(start - 50, start + 125)
        edict = dict(zip(ptexps, [0 for i in ptexps]))
        while edate < (end + 50):
            elist = [e for e, r in zip(ptexps, [random() for i in ptexps]) if r < 0.6]
            if not elist:
                continue
            for event in elist:
                if edict[event] > 15: rfprob = 0.05
                elif edict[event] > 5: rfprob = 0.15
                elif edict[event] > -5: rfprob = 0.8
                elif edict[event] > -15: rfprob = 0.5
                elif edict[event] > -50: rfprob = 0.15
                else: rfprob = 0.05
                if random() < rfprob:
                    days = supps[randint(0,len(supps) - 1)]
                    edict.update({event:days})
                    cprow = [pt, event, edate, days]
                    cpin.append(cprow)
            elapsed = randint(1, 10)
            for k in edict:
                edict.update({k:edict[k] - elapsed})
            edate += elapsed

    return cpin, bnds

# creates a counting process dataset from an event level file structured the same 
# way as that created by the 'mkcpin()' function above
def mkcp(incp, bounds, headers = True, mkbinary = True, printit = False, mkcsv = False, cpoutname = None, cpoutpath = None):
    from operator import itemgetter
    import csv
    
    if mkcsv:
        try:
            assert cpoutpath
        except AssertionError:
            print ("You must define an output path for the csv file in <CPOUTPATH>")
            return
            
    if headers:
        incp = incp[1:]
        bounds = bounds[1:]
        
    evs = sorted(set(i[1] for i in incp))
    evs.append('_dummy_')
    pts = sorted(set(i[0] for i in bounds))
    
    cp = []
    cphead = ['pt', 'winstart', 'winend', 'winlen']
    cphead.extend(evs[:-1])
    cp.append(cphead)
    
    for p in pts:
        ptbounds = [i for i in bounds if i[0] == p][0]
        ptdataraw = [i for i in incp if i[0] == p]
        
        # clean up ptdata -- rm / truncate stuff outside ptbounds 
        ptdata = []
        for row in ptdataraw:
            if type(row[2]) != int or type(row[3]) != int:
                continue
            elif row[2] <= 0 or row[3] <= 0:
                continue
            elif row[2] < ptbounds[1]:
                if row[2] + row[3] < ptbounds[1]:
                    continue
                else:
                    ptrow = [row[0], row[1], ptbounds[1], row[3] - (ptbounds[1] - row[2])]
                    ptdata.append(ptrow)
                    
            elif row[2] >= ptbounds[2]:
                continue
            elif row[2] + row[3] > ptbounds[2]:
                ptrow = [row[0], row[1], row[2], ptbounds[2] - row[2]]
                ptdata.append(ptrow)
            else:
                # data in this row are fine as-is, so append raw row
                ptdata.append(row)
        ptdataraw = []
        ptdata = sorted(sorted([i for i in ptdata], key = itemgetter(3), reverse = True), key = itemgetter(2), reverse = False)
        dummyrec = [p, '_dummy_', ptbounds[2], 0]
        ptdata.append(dummyrec)
        times = [0 for e in evs]
        ptdata_out = []
        status = []
        winstart = ptbounds[1]
        lastupdate = winstart
        basket = [0 for e in evs]
        for i, row in enumerate(ptdata):
            basket[evs.index(row[1])] += row[3]
            lastrec = (i == len(ptdata) - 1)
            newtime = False
            try:
                newtime = (row[2] < ptdata[i+1][2])
            except IndexError:
                pass
            if lastrec or newtime:
                gap = row[2] - lastupdate
                remaining = [[n, t] for n, t in enumerate(times) if 0 < t < gap]
                elapsed = 0
                while remaining:
                    minval = min([ri[1] for ri in remaining])
                    remaining = [x for x in remaining if x[1] > minval]
                    status = [p, winstart, (lastupdate + (minval - elapsed))]
                    status.extend(times) # pre-update
                    ptdata_out.append(status)
                    # update status as of <minval> time elapsed
                    times = [(x - (minval - elapsed)) for x in times]
                    elapsed = minval
                    winstart = status[2]
                    lastupdate = winstart
                # output the existing status *only* if the basket contains a new or restart exposure 
                anynew = False
                for t, s in enumerate(times):
                    if (times[t] - (gap - elapsed)) < 0 and basket[t] > 0:
                        anynew = True
                if anynew:
                    status = [p, winstart, row[2]]
                    status.extend(times)
                    ptdata_out.append(status)
                    winstart = row[2]
                for t, s in enumerate(times):
                    # dump basket into cabinet and decrement bottles that were not refilled 
                    if basket[t] > 0 and not lastrec:
                        times[t] = max((times[t] - (gap - elapsed)), basket[t])
                    elif not lastrec:
                        times[t] = times[t] - (gap - elapsed)
                basket = [0 for e in evs]
                lastupdate = row[2]
        status = [p, winstart, ptbounds[2]]
        status.extend(times)
        ptdata_out.append(status)
        for r in ptdata_out:
            r.insert(3, r[2] - r[1])
            if mkbinary:
                for c in range(4, len(r) - 1):
                    r[c] = (r[c] > 0) * 1
            cp.append(r[:-1])
        ptdata_out = []
        ptdata = []
        ptbounds = []
    if printit:
        chop(cp, 120)
    if mkcsv:
        if not cpoutname:
            cpoutname = 'cpout'
        with open('%s/%s.csv' % (cpoutpath, cpoutname), 'w', newline = '') as f:
            writer = csv.writer(f)
            writer.writerows(cp)
        print ("...created CSV output %s/%s.csv" % (cpoutpath, cpoutname))
        
    return cp

# 'smears' a counting process file (structured as created by mkcp()) so that
# small gaps in exposures are 'filled in' 
# NOTE: the output of this function is structured the same as the input
# for mkcp(), so after running smear(), run mkcp() on the output
def smear(cpinput, left_smear = 5, right_smear = 5):
    dhead = cpinput[0][4:]
    
    left_smear = left_smear or 5
    right_smear = right_smear or 5
    
    assert type(left_smear) in (int, dict)
    assert type(right_smear) in (int, dict)
    
    # note - left_smear and right_smear can be user-defined dictionaries with drug-specific smearing
    # if type is int then just create a dictionary that assigns the same value for smearing to all drugs
    if type(left_smear) == int: 
        sm = left_smear
        left_smear = {}
        for d in dhead:
            left_smear.update({d:sm})
    if type(right_smear) == int: 
        sm = right_smear
        right_smear = {}
        for d in dhead:
            right_smear.update({d:sm})            

    # should add ELSE checks to both of above (i.e., in the case of a user-defined dict) to
    # make sure drug names in dict match those in header (dhead)
    
    ptlist = []
    output_data = []
    ophead = ['ptid', 'event', 'evdate', 'evdays']
    output_data.append(ophead)
    
    for row in cpinput[1:]:
        if row[0] not in ptlist:
            pt = row[0]
            ptlist.append(pt)
            ptrowcount = sum([1 for x in cpinput if x[0] == pt]) # need a more efficient way to do this!
            memory = {}
            ptrow = 0
            
        ptrow += 1
        drugs = [i for i, d in enumerate(row[4:]) if d > 0]
        if drugs:
            starts = []
            for d in [x for x in drugs if x in list(memory)]:
                starts.append(memory[d][0])
                memory.update({d:[memory[d][0], row[2]]})
            # make a unique set of start times for pre-existing exposures, sorted from earliest to most recent
            starts = sorted([x for x in set(starts)])
            for d in [x for x in drugs if x not in list(memory)]:
                # d did not exist in the previous row - smear this exposure backwards in time
                # ... (i.e., to the 'left') to the earliest acceptable start day in starts 
                # if no acceptable start day exists in starts (i.e., starts is empty or all start 
                # ...days are too early, leave start day for d as-is (i.e., set it to winstart for this row)
                memory.update({d:[row[1], row[2]]})
                if starts:
                    for s in starts:
                        if row[1] - s <= left_smear[dhead[d]]:
                            memory.update({d:[s, row[2]]})
                            # this is the earliest acceptable start day, so do not look at more recent days
                            break
        # now deal with exposures in memory that are not present in the current row
        # if the right-smear associated with a given exposure will not carry 
        # ...it through the end of the current window, add it to the output dictionary 
        # ...and delete from memory
        # NOTE this step occurs regardless of whether this row is a gap or has exposures (drugs is not empty)
        for mk in list(memory):
            if mk not in drugs:
                if row[2] - memory[mk][1] > right_smear[dhead[mk]]:
                    # note - this output is actually input format for another run through mkcp
                    output_data.append([pt, dhead[mk], memory[mk][0], memory[mk][1] - memory[mk][0]])
                    del memory[mk]
                    
        if ptrow == ptrowcount:
            # done with records for this patient, so dump everything still in memory to the output file
            for mk in list(memory):
                output_data.append([pt, dhead[mk], memory[mk][0], row[2] - memory[mk][0]])
                del memory[mk]
                
    return output_data
 
# takes counting process files and makes 'lines of therapy' (really, exposures), which
# gets used by mkst() further down
def mklines(cpdata, keep_gaps = True, keep_gapzero = True):
    ptlevout = []
    head_ptlevout = ['pt', 'linenum', 'start', 'end', 'drugs']
    ptlevout.append(head_ptlevout)
    
    dhead = cpdata[0][4:]
    
    pts = []
    for row in cpdata[1:]:
        status = '/'.join(dhead[i] for i, x in enumerate(row[4:]) if x > 0) or 'gap'
        if row[0] not in pts:
            pts.append(row[0])
            lot = (status != 'gap') * 1 # starting line of therapy is 0 if pt starts in gap, 1 otherwise
        if status != 'gap' or keep_gaps:
            if status == 'gap' and lot == 0 and keep_gapzero is False: 
                # this is a gap occurring between the start of f/u and first treatment
                pass
            else:
                ptlevout.append([row[0], lot, row[1], row[2], status])
        lot += (status == 'gap') 
        # advance LOT by 1 every time person passes through gap
        # this is not super meaningful at the moment
    return ptlevout

# creates the source->target relationships for a Sankey diagram 
# from a counting process 'bounds' file and the ptlines output 
# from mklines() 
# NOTE: this is the function that really needs work
def mkst(
    ptlines,
    ptbounds,
    interv = [30, 60, 90, 180],
    minptrows = 1,
    rows_before_interv_end = 'LAST',
    subdiv_outcomes = False,
    expcoh = None,
    right_truncate = True
    ):
    
    source = []
    target = []
    value = []
    label = []
    
    srctarlist = []
    
    ptlist = []
    skippt = []
    
    tarnum = 0
    tardict = {}
    
    ncohorts = 0
    cohortlist = []
    expcoh = expcoh or None
    
    value_exp = []
    
    bracknumdict = {}
    ptrowdict = {}
    
    assert type(interv) == list
    assert len(interv) > 0
    
    istart = 1
    ints = []
    for i in interv:
        assert type(i) == int
        assert i > istart
        ints.append([istart, i])
        istart = i + 1
    interv = [i for i in ints]
    
    if type(rows_before_interv_end) == str:
        if rows_before_interv_end.upper() == 'LAST':
            rows_before_interv_end = -1
        else:
            rows_before_interv_end = 0
    #print ("NOTE: d/t settings, plot will show only patients with at least %s rows starting on or before day %s." % (str(rows_before_interv_end), str(interv[rows_before_interv_end][1])))
    # note - this first loop is used for storing the max number of events (for a given person) occurring within each interval - this allows things to line up in a sensible way in the plot
    # ... and for establishing # of rows per patient and which patients should be skipped
    
    for row in ptlines[1:]:
        if row[0] not in ptlist:
            #if row[2] != [p for p in ptbounds if p[0] == row[0]][1]:
            pt = row[0]
            #startday = row[2]
            ptboundsrow = [p for p in ptbounds if p[0] == pt][0]
            startday = ptboundsrow[1]
            if pt in skippt:
                continue
            ptrowcount = len([p for p in ptlines if p[0] == pt]) # not very efficient - should avoid this if possible...
            rbicount = len([p for p in ptlines if p[0] == pt and (p[2] - startday) <= interv[rows_before_interv_end][1]])
            ptrowdict.update({pt:ptrowcount})
            if rbicount < minptrows:
                skippt.append(pt)
                continue
            ptlist.append(pt)
            curr_bracket = None
            ncb = 0
            
        fuday = row[2] - startday + 1 # might make more sense to have f/u start at day 0 rather than day 1
        bracket = ['-'.join([str(i[0]), str(i[1])]) for i in interv if fuday >= i[0] and fuday <= i[1]]
        if bracket:
            bracket = bracket[0]
            if bracket != curr_bracket:
                ncb = 0
                curr_bracket = bracket
            ncb += 1
            if not bracknumdict.get(bracket):
                bracknumdict.update({bracket:ncb})
            elif bracknumdict.get(bracket) < ncb:
                bracknumdict.update({bracket:ncb})
                
    ptlist = []
    
    # now create source -> target relationships
    for row in ptlines[1:]:
        if row[0] not in ptlist:
            
            pt = row[0]
            if pt in skippt:
                continue
            ptrowcount = ptrowdict.get(pt)
            ptlist.append(pt)
            ptboundsrow = [p for p in ptbounds if p[0] == pt][0] # this is the whole row (list) for this patient, but w/o the [0], it will remain a list of (one) list
            startday = ptboundsrow[1] # event days/dates will be indexed to start day of follow-up (note this is the same day as row[2] for pt's first row in PTLINES)
            #startday = row[2] # use this instead of start day from ptbounds in case keep_gapzero was set to False
            src = ptboundsrow[4] # initial source is patient COHORT
            ptcoh = ptboundsrow[4]
            if src not in tardict:
                tardict.update({str(src):tarnum})
                tarnum += 1
                ncohorts += 1
                cohortlist.append(src)
                if not expcoh:
                    expcoh = src
                label.append(src)
            oc = ptboundsrow[3] # patient outcome
            oc_done = False
            ntimes = 1
            pttarlist = []
            ptrow = 0
            postinterv = None
            lastbracket = ['-'.join(str(i) for i in interv[0]), 0]
            
        ptrow += 1
        
        fuday = row[2] - startday + 1 # might make more sense to have f/u start at day 0 rather than day 1
        assert fuday > 0
        bracket = ['-'.join([str(i[0]), str(i[1])]) for i in interv if fuday >= i[0] and fuday <= i[1]]
        if not bracket:
            # start day for this window is beyond the end of the last interval (THIS IS AN ASSUMPTION ATM - NEED TO ADD SOME QC HERE!)
            # this will (or SHOULD!) be true for any subsequent rows for this patient as well
            # this row's target (row[4] and that of subsequent rows therfore won't be used as targets directly but can be used to further inform the outcome
            # for ex., if possible outcomes identified in ptbounds are 'lost to f/u', 'AMI', 'death', 'end-of-data', then we can further...
            # ...subdivide those outcomes into 'further treatment' vs. 'stopped treatment' (e.g., 'further treatment -> AMI' instead of just 'AMI') ...
            # ...if desired as a way to shed light on what happens after the last bracket but before the outcome.
            
            #print ("IS THIS EVEN HAPPENING??")
            if not postinterv:
                # this can only be true for the first post-interval record for this person
                # catch this person up with any missed nodes
                lastbrackpos = interv.index([int(i) for i in lastbracket[0].split('-')])
                currnode = lastbracket[1]
                for b in interv[lastbrackpos:]:
                    currbrack = '-'.join(str(x) for x in b)
                    nnodes = bracknumdict.get(currbrack)
                    if not nnodes:
                        continue
                    while currnode < nnodes:
                        currnode += 1
                        tarpre = 'pass:'
                        if currnode > 1:
                            tarpre = '---:'
                        tar = tarpre + currbrack + ':' + str(currnode)
                        st = str(src) + '->' + str(tar)
                        pttarlist.append(tar)
                        if tar not in tardict:
                            tardict.update({tar:tarnum})
                            tarnum += 1
                            label.append(tar)
                        if st in srctarlist:
                            value[srctarlist.index(st)] += 1
                            value_exp[srctarlist.index(st)] += (ptcoh == expcoh)
                        else:
                            srctarlist.append(st)
                            value.append(1)
                            value_exp.append((ptcoh == expcoh) * 1)
                            source.append(tardict.get(src))
                            target.append(tardict.get(tar))
                        src = tar
                    currnode = 0
                lastbracket = [None, 0]
                
            if row[4] == 'gap' and not postinterv:
                postinterv = 'stopped thx'
            else:
                postinterv = 'further thx'
            if ptrow == ptrowcount:
                bracket = ''
                if subdiv_outcomes:
                    oc = postinterv + '->' + oc
                tar = oc
                oc_done = True
            else:
                continue
        else:
            bracket = bracket[0]
            if bracket != lastbracket[0]:
                lastbrackpos = interv.index([int(i) for i in lastbracket[0].split('-')])
                currnode = lastbracket[1]
                for b in interv[lastbrackpos:interv.index([int(i) for i in bracket.split('-')])]:
                    # update up to, but not including, the current bracket (interval end is exclusive)
                    currbrack = '-'.join(str(x) for x in b)
                    nnodes = bracknumdict.get(currbrack)
                    if not nnodes:
                        continue
                    while currnode < nnodes:
                        currnode += 1
                        tarpre = 'pass:'
                        if currnode > 1:
                            tarpre = '---:'
                        tar = tarpre + currbrack + ':' + str(currnode)
                        st = str(src) + '->' + str(tar)
                        pttarlist.append(tar)
                        if tar not in tardict:
                            tardict.update({tar:tarnum})
                            tarnum += 1
                            label.append(tar)
                        if st in srctarlist:
                            value[srctarlist.index(st)] += 1
                            value_exp[srctarlist.index(st)] += (ptcoh == expcoh)
                        else:
                            srctarlist.append(st)
                            value.append(1)
                            value_exp.append((ptcoh == expcoh) * 1)
                            source.append(tardict.get(src))
                            target.append(tardict.get(tar))
                        src = tar
                    currnode = 0
                lastbracket = [bracket, 1]
            else:
                lastbracket = [bracket, lastbracket[1] + 1]
            tar = row[4] + ':' + bracket
            pttarcount = '/'.join(pttarlist).count(bracket)
            tar = tar + ':' + str(pttarcount + 1)
        # note: by default, ntimes = 1
        if ptrow == ptrowcount and not oc_done:
            ntimes = 2
        for i in range(ntimes):
            st = str(src) + '->' + str(tar)
            pttarlist.append(tar) # list of nodes this patient has passed through
            if tar not in tardict:
                tardict.update({tar:tarnum})
                tarnum += 1
                label.append(tar)
            if st in srctarlist:
                # this path has been followed by someone else
                value[srctarlist.index(st)] += 1 # iterate number of ppl who have followed this path by 1
                value_exp[srctarlist.index(st)] += (ptcoh == expcoh) # iterate num of ppl from exposed cohort 
            else:
                # this is a newly established path - make note of it and tack onto the end
                # ...of the respective lists
                srctarlist.append(st) # this is the master list of paths
                value.append(1)
                value_exp.append((ptcoh == expcoh) * 1)
                source.append(tardict.get(src))
                target.append(tardict.get(tar))
                
            src = tar
            if ntimes == 2:
                tar = oc
                
    colors = ["rgba(31, 119, 180, 0.8)" for x in target]
    
    # for each target, sum the number of people flowing in and the total number of people 
    # ...from the exposed cohort flowing in 
    # in the case that there are 2 starting cohorts, use this information to 
    #...create a color gradient for each of the targets (nodes) (e.g., from grapy to blue or 
    #...green to blue - note that paths are already gray, so gray may be a poor choice for a node)
    # green-to-blue gradient in 10 steps (google 'rgb color picker' and use    the built-in
    #...google tool!)
    colorbins = [
        [-0.1,0.1,"rgba(4,232,31,0.8)"],
        [0.1,0.2,"rgba(4,232,69,0.8)"],
        [0.2,0.3,"rgba(4,232,106,0.8)"],
        [0.3,0.4,"rgba(4,232,144,0.8)"],
        [0.4,0.5,"rgba(4,232,182,0.8)"],
        [0.5,0.6,"rgba(4,232,220,0.8)"],
        [0.6,0.7,"rgba(4,205,232,0.8)"],
        [0.7,0.8,"rgba(4,167,232,0.8)"],
        [0.8,0.9,"rgba(4,129,232,0.8)"],
        [0.9,1.0,"rgba(4,91,232,0.8)"]
    ]
    
    for tarnum, tarlab in enumerate(label):
        tot_inflow = 0
        tot_exp_inflow = 0
        if tarlab in cohortlist:
            if tarlab == expcoh:
                colors[tarnum] = colorbins[-1:][0][2] 
                # i.e., take the rgb param (2) from the last color bin, which is solid blue (100% exposed)
            else:
                colors[tarnum] = colorbins[0][2]
                # an unexposed cohort (note this could be >1 unique cohort) - solid green (0% exposed)
        elif tarlab.startswith('---:') or tarlab.startswith('pass:'):
            colors[tarnum] = "rgba(178,178,178,0.6)" # solid gray
            label[tarnum] = '...'
        else:
            # this is a rectangular node (or outcome), not a starting cohort, so will
            #...be somewhere on the color gradient, depending on makeup of inflowing ppl
            for i, path in enumerate(srctarlist):
                try:
                    if tarlab == path.split('->')[1]:
                        tot_inflow += value[i]
                        tot_exp_inflow += value_exp[i]
                except IndexError:
                    pass
            # all inflows for this tarlab have been summed, so pick a node color based on frac exposed
            frac_exp = float(tot_exp_inflow) / float(tot_inflow)
            colors[label.index(tarlab)] = [c[2] for c in colorbins if c[0] < frac_exp <= c[1]][0]
            
    plotdict = {
        'source':source,
        'target':target,
        'value':value,
        'nodelabel':label,
        'pathlabel':srctarlist,
        'color':colors
    }
    
    return plotdict

# makes a Sankey diagram using plotly and the output from mkst() above
# NOTE: this could (should) be converted to make Sankey diagrams using
# MatPlotLib instead
def mksankey(plot_dict, plot_title = 'Treatment Patterns'):
    
    srclist = plot_dict['source']
    tarlist = plot_dict['target']
    vallist = plot_dict['value']
    lablist = plot_dict['nodelabel']
    pathlist = plot_dict['pathlabel']
    colorlist = plot_dict['color']
    
    import plotly.graph_objects as go 
    fig = go.Figure(data = [go.Sankey(
        domain = dict(
            x = [0,1],
            y = [0,1]
        ),
        orientation = 'h',
        valueformat = '.0f',
        valuesuffix = ' pts',
        node = dict(
            pad = 15,
            thickness = 15,
            line = dict(
                color = 'black',
                width = 0.5
            ),
            label = lablist,
            color = colorlist
        ),
        link = dict(
            source = srclist,
            target = tarlist,
            value = vallist,
            label = pathlist
        ))])
    
    fig.update_layout(
        title = plot_title,
            font = dict(
                size = 10
            )
    )
    
    fig.write_html('test_sankey.html', auto_open = True)
    
    '''
    data = Data([trace1])
    data = Sankey(trace1)
    '''
        
    #fig = dict(data = [data_trace], layout = layout)
    ## NOTE: output_type = 'div' and displayHTML seem to be Databricks-specific.
    #plot_html = plot(fig, output_type = 'div')
        
    #displayHTML(plot_html)
    #import plotly.graph_objects as go
    #fig = go.Figure(data = go.Bar(y = [2, 3, 1]))
    #fig.write_html('first_figure.html', auto_open = True)
    
    '''
    fig = go.Figure(data=[go.Sankey(
        valueformat = ".0f",
        valuesuffix = "TWh",
        # Define nodes
        node = dict(
          pad = 15,
          thickness = 15,
          line = dict(color = "black", width = 0.5),
          label =  data['data'][0]['node']['label'],
          color =  data['data'][0]['node']['color']
        ),
        # Add links
        link = dict(
          source =  data['data'][0]['link']['source'],
          target =  data['data'][0]['link']['target'],
          value =  data['data'][0]['link']['value'],
          label =  data['data'][0]['link']['label'],
          color =  data['data'][0]['link']['color']
    ))])

    fig.update_layout(title_text="Energy forecast for 2050<br>Source: Department of Energy & Climate Change, Tom Counsell via <a href='https://bost.ocks.org/mike/sankey/'>Mike Bostock</a>",
                      font_size=10)
    fig.show()
    '''

    

    
