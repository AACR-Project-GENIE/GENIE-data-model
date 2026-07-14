# Description: Check for loss of PANC VICC patient.
# Author: Haley Hunter-Zinck
# Date: 2021-12-06

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_table_pt <- "syn21446700"
synid_file_upload <- "syn25541702"

# parameters
cohort = "PANC"
site = "VICC"

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

data <- get_synapse_entity_data_in_csv(synid_file_upload)

query <- glue("SELECT record_id FROM {synid_table_pt} WHERE cohort = '{cohort}' AND redcap_data_access_group = '{site}'")
id_tbl <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))


# main ----------------------------

id_upload <- as.character(unlist(data$record_id))
id_tbl <- as.character(unlist(id_tbl))

print(setdiff(id_tbl, id_upload))

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
