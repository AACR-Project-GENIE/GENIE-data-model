# Description: Check that elements have been removed from the Prostate Data Dictionary.  
# Author: Haley Hunter-Zinck
# Date: 2021-10-08
# Request:
#   Source: email from Jessica Lavery
#   Msg: variables that are no longer part of the Prostate data dictionary are below.  ensure they are not part of current dd.  
#   Date of Request: 2021-10-07

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_prostate_dd_curr <- "syn26260844"
synid_file_prostate_dd_prev <- "syn25570252"

# parameters
col_rm <- c("rt_phases", "rt_type_p1", "rt_planning_p1", "rt_dose_p1", "rt_site_p1", "rt_type_p2", "rt_planning_p2", 
"rt_dose_p2", "rt_site_p2", "rt_type_p3", "rt_planning_p3", "rt_dose_p3", "rt_site_p3")

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

dd_curr <- get_synapse_entity_data_in_csv(synapse_id = synid_file_prostate_dd_curr, 
                                     na.strings = c(""), 
                                     header = T) 
dd_prev <- get_synapse_entity_data_in_csv(synapse_id = synid_file_prostate_dd_prev, 
                                          na.strings = c(""), 
                                          header = T)

# main ----------------------------

n_rm <- length(col_rm)
n_curr <- length(intersect(dd_curr$`Variable / Field Name`, col_rm))
n_prev <- length(intersect(dd_prev$`Variable / Field Name`, col_rm))

print(glue("Number of columns present in previous DD: {n_prev} of {n_rm}"))
print(glue("Number of columns present in current DD: {n_curr} of {n_rm}"))

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
