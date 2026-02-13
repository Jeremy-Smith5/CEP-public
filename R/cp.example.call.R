## example call for cp.R
## NOTE: .csv files used below are derived from the 
## synthetic version of the Prescription Drug Event (PDE)
## file released on the CMS website.  The two files 
## below are in the folder 'sample_data' located in this repo.
## J. Smith Feb 2026

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
