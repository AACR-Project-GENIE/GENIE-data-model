# Description: Check the DFCI upload to see if header and data columns match. 
# Author: Haley Hunter-Zinck
# Date: 2021-12-21

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_data <- "syn26230815"
synid_file_header <- "syn25592217"

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

data <- get_synapse_entity_data_in_csv(synid_file_data)
header <- as.character(read.csv(synGet(synid_file_header)$path, check.names = F,
                                na.strings = c(""),
                                stringsAsFactors = F))

# close out ----------------------------

print(glue("Number of columns in data matrix: {ncol(data)}"))
print(glue("Length of header data: {length(header)}"))

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
