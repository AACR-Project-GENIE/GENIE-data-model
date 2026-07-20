# Description: Check samples in SCI and PROV to examine overlap.
# Author: Haley Hunter-Zinck
# Date: 2022-04-07

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)

# synapse
synid_file_sci_cli <- "syn17017899"
synid_file_prov_pat <- "syn26986939"
synid_file_prov_sam <- "syn26986940"

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

synLogin()

# read ----------------------------

pat_prov <- get_synapse_entity_data_in_csv(synid_file_prov_pat, sep = "\t")
sam_prov <- get_synapse_entity_data_in_csv(synid_file_prov_sam, sep = "\t")
cli_prov <- pat_prov %>%
  inner_join(sam_prov, by = c("PATIENT_ID"))

cli_sci <- get_synapse_entity_data_in_csv(synid_file_sci_cli, sep = "\t")

# main ----------------------------

tbl_center_prov <- table(unlist(lapply(strsplit(split = "-", cli_prov$PATIENT_ID), function(x) {x[2]})))
tbl_center_sci <- table(unlist(lapply(strsplit(split = "-", cli_sci$PATIENT_ID), function(x) {x[2]})))

print(tbl_center_prov)
print(tbl_center_sci)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
