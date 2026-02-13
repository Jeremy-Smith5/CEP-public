
## build a counting process dataframe given input containing patient events and boundaries of follow-up
## ...this is a work in progress - use with great skepticism
## Jeremy Smith
## Feb 2026

## EXAMPLE CALL:


cp <- function(
    cpevents, # R dataframe (<ptid> startdate enddate event edate days)
    cpbounds, # [optional] R dataframe: unique(<ptid> startdate enddate) - if not prov., created from cpevents
    ptid = "ptid" , # patient identifier
    dupeventaction = "sum", # handling days var for same event, same edate
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
    print("NOTE: ...cpbounds dataframe will be created from cpevents")
  }
   cpeventvars <- names(cpevents)
   cpeventvars_req <- c(ptid, "event", "edate", "days")
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
    print(sprintf("WARNING: %s patients will be dropped d/t invalid start or end date", nbadbounds))
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

    cabinet <- rep(0, length(eventvec))
    names(cabinet) <- eventvec

    # extract events for this person
    pti_events <- cpevents[cpevents[[ptid]] == pti, 2:ncol(cpevents)]

    # drop any records that have NA values
    pti_events <- pti_events[complete.cases(pti_events), ]
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
    pti_events[nrow(pti_events)+1, ] <- list("cpstop", endti+15000, 1)    
    pti_edates <- sort(unique(pti_events$edate))
    prior_edate <- stdti
    ptcp <- list()
    ptcprow <- 0

    n_edates <- length(pti_edates)
    
    if (n_edates == 0) {
      # patient had no events within start/end boundaries - 
      # ...output a single record showing no exposure
      ptcp[[1]] <- data.frame(
        person=pti, 
        startdate=stdti, 
        enddate=endti, 
        winstart=stdti, 
        winend=endti,
        len = as.integer(endti - stdti),
        as.list(cabinet),
        stringsAsFactors = FALSE
      )      
    } else {
      for (edtnum in 1:n_edates) {
        
        edt <- pti_edates[[edtnum]]
  
        # record any (ON->OFF) state changes that have occurred since prior_edate and before edt
        while (any(cabinet > 0 & cabinet < (edt - prior_edate))) {
          ptcprow <- ptcprow + 1
  
          minval <- as.integer(min(cabinet[cabinet > 0 & cabinet < (edt - prior_edate)]))

  
          ws <- prior_edate
          prior_edate <- pmin(endti, ws + minval)

          ptcp[[ptcprow]] <- data.frame(
            person=pti, 
            startdate=stdti, 
            enddate=endti, 
            winstart=ws, 
            winend=prior_edate,
            len = as.integer(prior_edate - ws),
            as.list(cabinet),
            stringsAsFactors = FALSE
            )
          
          if (prior_edate == endti) { break }
          cabinet <- cabinet - minval
          
        }
  
        if (edtnum == n_edates) { break }
        
        pti_edate_events <- pti_events[pti_events$edate == edt, c("event", "days")]
        pti_edate_events <- setNames(pti_edate_events$days, pti_edate_events$event)
        pti_edate_events <- tapply(pti_edate_events, names(pti_edate_events), dupeventaction)
        
        basket <- rep(0, length(eventvec))
        names(basket) <- eventvec
        basket[names(pti_edate_events)] <- pti_edate_events

        offons <- which(cabinet <= 0 & basket > 0)
        onons <- which(cabinet > 0 & basket > 0)

        gap <- as.integer(edt - prior_edate)

        if (length(offons) && edt > prior_edate) {
          # output the latent record and prepare for new exposure
          ptcprow <- ptcprow + 1
          ptcp[[ptcprow]] <- data.frame(
            person = pti,
            startdate = stdti,
            enddate = endti,
            winstart = prior_edate,
            winend = edt,
            len = gap,
            as.list(cabinet),
            stringsAsFactors = FALSE
          )
          prior_edate <- edt
          cabinet <- cabinet - gap
        }

        for (n in 1:length(cabinet)) {
          if (n %in% offons) {
            cabinet[n] <- pmax(cabinet[n], basket[n])
          } else if (n %in% onons) {
            cabinet[n] <- sum(cabinet[n], basket[n])
          }
        }

      }  # end edate loop
    }
    
    # stack all rows for this patient
    allcp[[ptnum]] <- do.call(rbind, ptcp)
    
  }  # end patient loop
  
  # stack all patients
  outcp <- do.call(rbind, allcp)
  
  # clean up
  outcp$cpstop <- NULL
  names(outcp)[1] <- ptid
  
  eventvec <- eventvec[-length(eventvec)]
  
  if (cleanup) {
    # convert all cabinet values to 0/1
    outcp[eventvec] <- (outcp[eventvec] > 0) * 1
  }
  
  return(outcp)
  
}

