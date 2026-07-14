# Description: examine inconsistencies from complex query 605 at VICC.
# Author: Haley Hunter-Zinck
# Date: August 12, 2021
# email:
#   date: August 12, 2021
#   from: Jessica Lavery
#   to: Haley Hunter-Zinck and Xindi Guo
# conclusion: "path_qamajor___9"  "path_qaminor___4" not in June table version

# setup -------------------------------------

tic = as.double(Sys.time())

library(synapser)
synLogin()
library(glue)

# synapse
synid_dir_tables = "syn21446696"

# dates
cutdate_july = "2021-07-16"
cutdate_june <- "2021-06-16"

# functions ------------------------------

get_prev_version_num <- function(table_id, cohort_date_cut) {
  version_history_list = synRestGET(glue("/entity/{table_id}/version?limit=50"))
  version_history = as.data.frame(do.call(rbind,version_history_list$results))
  version_history$date_cut <- sapply(version_history$modifiedOn, function(x) as.character(as.Date(as.POSIXct(x, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS"), tz = "America/Los_Angeles")))
  return(version_history$versionNumber[version_history$date_cut==cohort_date_cut])
}

now <- function(timeOnly = F, tz = "US/Pacific") {
  
  Sys.setenv(TZ = tz)
  
  if (timeOnly) {
    return(format(Sys.time(), "%H:%M:%S"))
  }
  
  return(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
}

# examine columns ------------------------

added <- list()
removed <- list()

data_table_info <- as.data.frame(synTableQuery(glue("SELECT id, name, form, primary_key FROM {synid_dir_tables} WHERE double_curated = 'false'"),includeRowIdAndRowVersion=FALSE))

for (i in 1:nrow(data_table_info)) {
  
  table_id <- data_table_info$id[i]
  cat(glue("{now()}: Table id {table_id} ({i} / {nrow(data_table_info)})"), "\n")
  
  version_july <- get_prev_version_num(table_id, cohort_date_cut = cutdate_july)[[1]]
  version_june <- get_prev_version_num(table_id, cohort_date_cut = cutdate_june)[[1]]
  
  col_july <- colnames(as.data.frame(synTableQuery(glue("SELECT * FROM {table_id}.{version_july} LIMIT 1"))))
  col_june <- colnames(as.data.frame(synTableQuery(glue("SELECT * FROM {table_id}.{version_june} LIMIT 1"))))
  
 added[[table_id]] <- setdiff(col_july, col_june)
 removed[[table_id]] <- setdiff(col_june, col_july)
}

print(added)
print(removed)
lapply(added,length)
lapply(removed,length)

