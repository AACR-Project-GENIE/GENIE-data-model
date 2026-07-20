# Description: Compare patient and sample lists in new and updated ERBB2 data.
# Author: Haley Hunter-Zinck
# Date: 2022-05-03

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)

# synapse
synid_file_cli_old <- "syn8384867"
synid_file_cli_new <- "syn30042103"
synid_file_cli_mg <- "syn26706689"
synid_file_cli_ng <- "syn30041987"
synid_file_mut_old <- "syn8385102"

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
#' @param comment.char character designating comment lines to ignore
#' @return data frame
get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA,
                                           sep = ",", 
                                           na.strings = c("NA"), 
                                           header = T,
                                           check_names = F,
                                           comment.char = "#",
                                           colClasses = "character") {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = check_names,
                   header = header, comment.char = comment.char, colClasses = colClasses)
  return(data)
}

# synapse login --------------------

status <- synLogin()

# read ----------------------------

old_cli <- get_synapse_entity_data_in_csv(synid_file_cli_old, sep = "\t", na.strings = c("", "NA"))
old_mut <- get_synapse_entity_data_in_csv(synid_file_mut_old, sep = "\t", na.strings = c("", "NA"))


new_cli <- get_synapse_entity_data_in_csv(synid_file_cli_new, sep = "\t", na.strings = c("", "NA"))
mg_cli <- get_synapse_entity_data_in_csv(synid_file_cli_mg, sep = "\t", na.strings = c("", "NA"))
ng_cli <- get_synapse_entity_data_in_csv(synid_file_cli_ng, sep = "\t", na.strings = c("", "NA"))

# patient ids ----------------------------

old_pt_id <- as.character(unlist(old_cli %>% select(PATIENT_ID)))
new_pt_id <- as.character(unlist(new_cli %>% select(PATIENT_ID)))
mg_pt_id <- as.character(unlist(mg_cli %>% select(PATIENT_ID)))
ng_pt_id <- as.character(unlist(ng_cli %>% select(PATIENT_ID)))

new_not_old <- setdiff(new_pt_id, old_pt_id)
old_not_new <- setdiff(old_pt_id, new_pt_id)

old_not_mg <- setdiff(old_pt_id, mg_pt_id)
new_not_mg <- setdiff(new_pt_id, mg_pt_id)
identical(sort(old_not_mg), sort(new_not_mg))

new_not_mg_not_ng <- setdiff(new_not_mg, ng_pt_id)

# mutations ----------------------------

old_mut_sam_id <- old_mut$Tumor_Sample_Barcode
for (pat_id in new_not_mg_not_ng) {
  print(grep(x = old_mut_sam_id, pattern = pat_id, value = T))
}

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
