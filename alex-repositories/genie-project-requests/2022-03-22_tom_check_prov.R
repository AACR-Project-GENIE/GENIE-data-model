# Description: Check PROV sample IDs in all files.  
# Author: Haley Hunter-Zinck
# Date: 2022-03-22

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_pat <- "syn26986939"
synid_file_sam <- "syn26986940"
synid_file_mut <- "syn26862822"

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
                                           comment.char = "#") {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = check_names,
                   header = header, comment.char = comment.char)
  return(data)
}

# read ----------------------------

pat <- get_synapse_entity_data_in_csv(synid_file_pat, sep = "\t")
sam <- get_synapse_entity_data_in_csv(synid_file_sam, sep = "\t")
mut <- get_synapse_entity_data_in_csv(synid_file_mut, sep = "\t")

# main ----------------------------

# get IDs
pat_pat_ids <- unlist(pat %>% select(PATIENT_ID) %>% distinct())
sam_sam_ids <- unlist(sam %>% select(SAMPLE_ID) %>% distinct())
sam_pat_ids <- unlist(sam %>% select(PATIENT_ID) %>% distinct())
mut_sam_ids <- unlist(mut %>% select(Tumor_Sample_Barcode) %>% distinct())

# count any PATIENT IDs in sample file are in the patient file
n_pat_not_sam <- length(setdiff(sam_pat_ids, pat_pat_ids))

# count any SAMPLE IDs in mutation file but not sample file
n_mut_not_sam <- length(setdiff(mut_sam_ids, sam_sam_ids))

# close out ----------------------------

# report out
print(glue("Number of patient IDs in '{synGet(synid_file_pat, downloadFile = F)$properties$name}' ({synid_file_pat}) but not '{synGet(synid_file_sam, downloadFile = F)$properties$name}' ({synid_file_sam}): {n_pat_not_sam}"))
print(glue("Number of samples IDs in '{synGet(synid_file_sam, downloadFile = F)$properties$name}' ({synid_file_sam}) but not '{synGet(synid_file_mut, downloadFile = F)$properties$name}' ({synid_file_mut}): {n_mut_not_sam}"))

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
