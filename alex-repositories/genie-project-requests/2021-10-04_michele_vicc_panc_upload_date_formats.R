# Description: Check date formats marked as incorrect in the QA report.  
# Author: Haley Hunter-Zinck
# Date: 2021-10-04

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_vicc_panc <- "syn25541702"

# functions ----------------------------

get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA,
                                           sep = ",", 
                                           na.strings = c("NA"), 
                                           header = T) {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = F,
                   header = header)
  return(data)
}

# read ----------------------------

data_curr <- get_synapse_entity_data_in_csv(synid_file_vicc_panc, version = 3)
data_prev <- get_synapse_entity_data_in_csv(synid_file_vicc_panc, version = 2)

# main ----------------------------

column_names <- c("qa_full_date", "curation_dt", "pt_start_time", "drugs_stop_time")

print("Current version date formats:")
for (column_name in column_names) {
  print(column_name)
  print(head(data_curr[[column_name]][data_curr[[column_name]] != ""]))
  print("-----")
}

print("Previous version date formats:")
for (column_name in column_names) {
  print(column_name)
  print(head(data_curr[[column_name]][data_curr[[column_name]] != ""]))
  print("-----")
}

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
