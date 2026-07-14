# Description: Check BLADDER eligible case lists.  
# Author: Haley Hunter-Zinck
# Date: 2021-11-16

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_old <- "syn22311993"
synid_file_new <- "syn26467942"
synid_file_mat <- "syn26467941"

# functions ----------------------------

#' Download and load data stored in csv or other delimited format on Synapse
#' into an R data frame.
#' 
#' @param synapse_id Synapse ID
#' @version Version of the Synapse entity to download.  NA will load current
#' version
#' @param set Delimiter for file
#' @param na.strings Vector of strings to be read in as NA values
#' @param header TRUE if the file contains a header row; FALSE otherwise.
#' @param check_names TRUE if column names should be modified for compatibility 
#' with R upon reading; FALSE otherwise.
#' @return data frame
get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA,
                                           sep = ",", 
                                           na.strings = c("NA"), 
                                           header = T,
                                           check_names = F) {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = check_names,
                   header = header)
  return(data)
}

# read ----------------------------

old <- get_synapse_entity_data_in_csv(synid_file_old)
new <- get_synapse_entity_data_in_csv(synid_file_new)
mat <- get_synapse_entity_data_in_csv(synid_file_mat)

# main ----------------------------

pt_old <- unlist(old %>% filter(is.na(remove)) %>% select(patient_id))
pt_new <- new$PATIENT_ID

print(length(pt_old))
print(length(pt_new))
print(length(intersect(pt_old, pt_new)))
print(setdiff(pt_old, pt_new))
print(setdiff(pt_new, pt_old))

mat %>% 
  filter(is.element(PATIENT_ID, setdiff(pt_new, pt_old)))

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
