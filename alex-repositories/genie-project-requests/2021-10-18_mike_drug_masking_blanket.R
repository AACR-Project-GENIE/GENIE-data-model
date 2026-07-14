# Description: For regimens marked as being a part of a clincial trial, what number
#   of unique drug names are unmasked?
# Author: Haley Hunter-Zinck
# Date: 2021-10-18

# user input ----------------------------

args <- commandArgs(trailingOnly = T)
waitifnot <- function(cond, msg) {
  if (!cond) {
    
    for (str in msg) {
      message(str)
    }
    message("Press control-C to exit and try again.")
    
    while(T) {}
  }
}
waitifnot(cond = length(args) == 1, 
          msg = "Usage: R -f 2021-10-18_mike_drug_masking_blanket.R --args <synid_file_drug_report>")

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
#synid_file <- "syn26214388"
synid_file <- args[1]

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

get_synapse_entity_name <- function(synapse_id) {
  return(synGet(synapse_id, downloadFile = F)$properties$name)
}

# read ----------------------------

data <- get_synapse_entity_data_in_csv(synid_file)

# main ----------------------------

reg_ct <- as.character(unlist(data %>%
  filter(drugs_ct_yn == "Yes") %>%
  select(drugs_in_regimen) %>% 
  distinct()))

drugs_ct <- unique(unlist(strsplit(reg_ct, split = ", ")))

# close out ----------------------------

print(glue("Analyzed regimens in file '{get_synapse_entity_name(synid_file)}' ({synid_file})"))
print(glue("Number of unique unmasked drug names in clinical trial regimens: {length(drugs_ct)}"))

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
