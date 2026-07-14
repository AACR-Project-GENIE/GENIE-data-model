# Description: Explore BPC custom cBioPortal files. 
# Author: Haley Hunter-Zinck
# Date: 2022-03-21

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_timeline_imaging <- "syn26349085"
synid_file_timeline_labtest <- "syn26436906"

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

timeline_imaging <- get_synapse_entity_data_in_csv(synid_file_timeline_imaging, sep = "\t")
timeline_labtest <- get_synapse_entity_data_in_csv(synid_file_timeline_labtest, sep = "\t")

# main ----------------------------

timeline_imaging %>% select(EVENT_TYPE) %>% distinct()
timeline_labtest %>% select(EVENT_TYPE) %>% distinct()

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
