## example call for cp.R
## NOTE: .csv files used below are derived from the 
## synthetic version of the Prescription Drug Event (PDE)
## file released on the CMS website.  The two files 
## below are in the folder 'sample_data' located in this repo.
## J. Smith Feb 2026

# EXAMPLE #2 - using synthetic PDE as described above
bounds <- read.csv(
  "/path/to/data/synthetic_cp_bounds.csv", 
  header = TRUE, 
  sep = ",", 
  stringsAsFactors = FALSE,
  colClasses = c("ptid" = "character")
)

events <- read.csv(
  "/path/to/data/synthetic_cp_events.csv", 
  header = TRUE, 
  sep = ",", 
  stringsAsFactors = FALSE,
  colClasses = c("ptid" = "character")
)

cpdata <- cp(
      events, # R dataframe (<ptid> startdate enddate event edate days)
      bounds, # [optional] R dataframe: unique(<ptid> startdate enddate) - if not prov., created from cpevents
      ptid = "ptid" , # patient identifier
      dupeventaction = "sum" # handling days var for same event, same edate
  )

## EXAMPLE # 2 -- demonstrating inclusion of a lab-type event and an outcome event
indata <- data.frame(
  ptid=c(101, 101, 101, 101, 101, 102, 102, 102, 102),
  startdate=as.Date(c(50, 50, 50, 50, 50, 300, 300, 300, 300)),
  enddate=as.Date(c(160, 160, 160, 160, 160, 450, 450, 450, 450)),
  event=c("A", "C", "C", "A", "C", "D", "C", "outcome_fail", "C"),
  edate=as.Date(c(58, 70, 78, 92, 150, 328, 328, 340, 390)),
  days=c(30, 15, 15, 45, 15, 28, 15, 1, 15),
  labval=c(NA, 22.9, 30.8, NA, 24.5, NA, 42.2, NA, 40.7)
)

  source("/path/to/cp/cp.R")
  
cpdata <- cp(
  indata, # R dataframe (<ptid> startdate enddate event edate days)
  # cpbounds: [optional] R dataframe: unique(<ptid> startdate enddate) - if not prov., created from cpevents
  ptid = "ptid" , # patient identifier
  dupeventaction = "sum" # handling days var for same event, same edate
)
  
print(head(cpdata, 50))

