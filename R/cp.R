
## build a counting process dataframe given input containing patient events and boundaries of follow-up
## ...this is a work in progress - use with great skepticism
## Jeremy Smith
## Feb 2026

## see example call in this repo

cp <- function(
    cpevents, # R dataframe (<ptid> startdate enddate event edate days)
    cpbounds, # [optional] R dataframe: unique(<ptid> startdate enddate) - if not prov., created from cpevents
    ptid = "ptid" , # patient identifier
    dupeventaction = "max", # handling days var for same event, same edate
    cleanup = TRUE # convert all 'cabinet' values to 0/1
) {

  cpeventsOK <- FALSE
  if (exists(as.character(substitute(cpevents)))) {if (is.data.frame(get("cpevents"))) {cpeventsOK <- TRUE}}
  if (!cpeventsOK) {
    stop(print("...you must provide a valid dataframe for <cpevents>!"))
  }
  cpboundsOK <- FALSE

  if (!missing(cpbounds)) {if (is.data.frame(get("cpbounds"))) {cpboundsOK <- TRUE}}
  if (!cpboundsOK) {
    message("NOTE: ...cpbounds dataframe will be created from cpevents")
  }
  cpeventvars <- names(cpevents)
  cpeventvars_req <- c(ptid, "event", "edate", "days")
  
  anylabs <- FALSE
  if ("labval" %in% cpeventvars) {
    labevents <- unique(cpevents[!is.na(cpevents$labval), "event"])
    if (length(labevents)) {
      anylabs <- TRUE
      cpeventvars_req[5] <- "labval"
    }
  }
  
  if (!all(cpeventvars_req %in% cpeventvars)) {
    stop(sprintf("...the cpevents df must contain: [%s]", cpeventvars_req))
  }
  else if (!cpboundsOK) {
    cpbounds <- cpevents
  }
  
  cpboundvars_req <- c(ptid, "startdate", "enddate")
  cpboundvars <- names(cpbounds)
  if (!all(cpboundvars_req %in% cpboundvars)) {
    stop(sprintf("...the cpbounds df must contain: [%s]", cpboundvars_req))
  }
  
  cpevents <- cpevents[cpeventvars_req]

  cpbounds <- cpbounds[!duplicated(cpbounds[, cpboundvars_req]), ]

  cpbounds <- cpbounds[order(cpbounds[[ptid]], cpbounds[["startdate"]]), ]
  if (nrow(cpbounds) != length(unique(cpbounds[[ptid]]))) {
    stop(print("...the cpbounds df must have one row/person with each person having a single start and end date"))
  }

  nbadbounds <- nrow(
    cpbounds[(cpbounds$enddate <= cpbounds$startdate) | 
                (any(is.na(c(cpbounds$startdate, cpbounds$enddate)))), ])

  if (nbadbounds > 0) {
    message(sprintf("WARNING: %s patients will be dropped d/t invalid start or end date", nbadbounds))
  }

  cpbounds <- cpbounds[(cpbounds$enddate > cpbounds$startdate &
                          !any(is.na(c(cpbounds$startdate, cpbounds$enddate)))), ]
  
  npts <- nrow(cpbounds)
  eventvec <- sort(unique(cpevents$event))
  eventvec[[length(eventvec) + 1]] <- "cpstop"

  allcp <- list()

  for (ptnum in 1:npts) {
    
    pti <- cpbounds[ptnum, ptid]
    stdti <- cpbounds[ptnum, "startdate"]
    endti <- cpbounds[ptnum, "enddate"]
    
    # extract events for this person
    pti_events <- cpevents[cpevents[[ptid]] == pti, 2:ncol(cpevents)]

    ocrecs <- which(grepl("^outcome_", pti_events$event))
    if (length(ocrecs)) {
      minocdate <- as.Date(min(pti_events[ocrecs, "edate"]))
      endti <- pmin(endti, minocdate + 1)
    }

    cabinet <- rep(0, length(eventvec))
    names(cabinet) <- eventvec
    
    if (anylabs) {
      labs_cabinet <-rep(NA, length(eventvec))
      names(labs_cabinet) <- paste(eventvec, "value", sep = ".")
    }

    # drop any records that have NA values (ignoring lab values)
    pti_events <- pti_events[complete.cases(pti_events[c("event", "edate", "days")]), ]
    # round decimal days to whole numbers
    pti_events$days <- round(pti_events$days)

    # left truncate events that occur prior to patient start date
    pti_events$days <- pmax(0, pti_events$days - pmax(0, stdti - pti_events$edate))

    # reset date of left-truncated events to patient start date
    pti_events$edate <- pmax(pti_events$edate, stdti)
    # right truncate exposures that persist beyond patient end date
    pti_events$days <- pmin(pti_events$days, endti - pti_events$edate)

    # keep only resulting records where persistence (days) is > 0 and event date is <= patient end date
    pti_events <- pti_events[pti_events$days > 0 & pti_events$edate <= endti, ]

    pti_events <- pti_events[order(pti_events$edate, pti_events$event), ]
    
    dummyrow <- list("cpstop", endti + 15000, 1)
    if (anylabs) { dummyrow[[4]] <- NA }
    pti_events[nrow(pti_events)+1, ] <- dummyrow
    
    pti_edates <- sort(unique(pti_events$edate))
    prior_edate <- stdti
    ptcp <- list()
    ptcprow <- 0

    n_edates <- length(pti_edates)
    
    if (n_edates == 0) {
      # patient had no events within start/end boundaries - 
      # ...output a single record showing no exposure
      outrow <- list(
        person=pti, 
        startdate=stdti, 
        enddate=endti, 
        winstart=stdti, 
        winend=endti,
        len = as.integer(endti - stdti)
      )
      outrow <- c(outrow, as.list(cabinet))

      if (anylabs) { outrow <- c(outrow, as.list(labs_cabinet)) }
      ptcp[[1]] <- data.frame(outrow, stringsAsFactors = FALSE)
    } else {
      for (edtnum in 1:n_edates) {
        
        edt <- pti_edates[[edtnum]]
  
        # record any (ON->OFF) state changes that have occurred since prior_edate and before edt
        while (any(cabinet > 0 & cabinet < (edt - prior_edate))) {
          ptcprow <- ptcprow + 1
  
          minval <- as.integer(min(cabinet[cabinet > 0 & cabinet < (edt - prior_edate)]))

          ws <- prior_edate
          prior_edate <- pmin(endti, ws + minval)

          outrow <- list(
            person=pti, 
            startdate=stdti, 
            enddate=endti, 
            winstart=ws, 
            winend=prior_edate,
            len = as.integer(prior_edate - ws)
          )
          outrow <- c(outrow, as.list(cabinet))

          if (anylabs) {
            minlocs <- which(cabinet == minval)
            outrow <- c(outrow, as.list(labs_cabinet))
            labs_cabinet[minlocs] <- NA
            }
          ptcp[[ptcprow]] <- data.frame(outrow, stringsAsFactors = FALSE)
          
          if (prior_edate == endti) { break }
          cabinet <- cabinet - minval
          
        }
  
        if (edtnum == n_edates && prior_edate == endti) { break }
        
        pti_edate_events <- pti_events[pti_events$edate == edt, c("event", "days")]
        pti_edate_events <- setNames(pti_edate_events$days, pti_edate_events$event)
        pti_edate_events <- tapply(pti_edate_events, names(pti_edate_events), dupeventaction)
        
        basket <- rep(0, length(eventvec))
        names(basket) <- eventvec
        basket[names(pti_edate_events)] <- pti_edate_events
        
        offons <- which(cabinet <= 0 & basket > 0)
        onons <- which(cabinet > 0 & basket > 0)
        
        if (anylabs) {
          pti_edate_labs <- pti_events[pti_events$edate == edt, c("event", "labval")]
          pti_edate_labs <- setNames(pti_edate_labs$labval, paste(pti_edate_labs$event, "value", sep = "."))
          pti_edate_labs <- tapply(pti_edate_labs, names(pti_edate_labs), max)
          
          labs_basket <- rep(NA, length(eventvec))
          names(labs_basket) <- names(labs_cabinet)
          labs_basket[names(pti_edate_labs)] <- pti_edate_labs
          
          if (any(!is.na(pti_edate_labs))) {

            # for the members of labs_basket that are not NA, identify all corresponding values of 
            # labs_cabinet that are not the same (either NA or unequal value) -- these will be the 
            # off->ons for the labs
            compCB <- (labs_basket[!is.na(labs_basket)] != labs_cabinet[!is.na(labs_basket)] 
                       | is.na(labs_cabinet[!is.na(labs_basket)]))
            
            ovrwrite <- setNames(rep(FALSE, length(labs_basket)), names(labs_basket))
            ovrwrite[names(compCB)] <- compCB

            # re-create the off->on vector, now incorporating the information in ovrwrite
            offons <- which((cabinet <= 0 & basket > 0) | ovrwrite)
          }
        }

        wend <- pmin(endti, edt)
        gap <- as.integer(wend - prior_edate)

        if (length(offons) && edt > prior_edate) {
          # output the latent record and prepare for new exposure
          ptcprow <- ptcprow + 1
          outrow <- list(
            person = pti,
            startdate = stdti,
            enddate = endti,
            winstart = prior_edate,
            winend = wend,
            len = gap
          )
          outrow <- c(outrow, as.list(cabinet))

          if (anylabs) {
            outrow <- c(outrow, as.list(labs_cabinet))
            labs_cabinet[!is.na(labs_basket)] <- labs_basket[!is.na(labs_basket)]
          }

          ptcp[[ptcprow]] <- data.frame(outrow, stringsAsFactors = FALSE)
          
          if (edtnum == n_edates) { break }
          
          prior_edate <- edt
          cabinet <- cabinet - gap
        }

        # unload basket into cabinet
        for (n in 1:length(cabinet)) {
          if (n %in% offons) {
            cabinet[n] <- pmax(cabinet[n], basket[n])
          } else if (n %in% onons) {
            cabinet[n] <- sum(cabinet[n], basket[n])
          }
        }

      }  # end edate loop
      
    }  # end else block
    
    # stack all rows for this patient
    allcp[[ptnum]] <- do.call(rbind, ptcp)
    
  }  # end patient loop
  
  # stack all patients
  outcp <- do.call(rbind, allcp)
  
  # clean up
  outcp$cpstop <- NULL
  names(outcp)[1] <- ptid
  if (anylabs) {
    valnames <- names(outcp[grepl("\\.value$", names(outcp))])
    labevents <- paste(labevents, "value", sep = ".")
    valnames <- valnames[labevents != valnames]
    outcp[valnames] <- NULL
  }
  
  eventvec <- eventvec[-length(eventvec)]
  
  if (cleanup) {
    # convert all cabinet values to 0/1
    outcp[eventvec] <- (outcp[eventvec] > 0) * 1
  }
  
  return(outcp)
  
}



