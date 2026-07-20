# Description: Verify invalid codes in DFCI BLADDER upload.  
# Author: Haley Hunter-Zinck
# Date: 2022-03-15

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_data <- "syn26250078"
synid_file_header <- "syn26250090"

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

header <- get_synapse_entity_data_in_csv(synid_file_header)
data <- get_synapse_entity_data_in_csv(synid_file_data)
colnames(data) <- header

# main ----------------------------

data %>%
  filter(path_cauncert_hist1 == "8001a") %>%
  count()

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
