
def tbl2pylist(tbl):
    pylist = sqlContext.sql("select * from %s" % tbl)
    pylist = pylist.toPandas()
    pylist = pylist.values.tolist()
    
    return pylist
    
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
