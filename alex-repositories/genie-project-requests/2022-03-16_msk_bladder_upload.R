# Description: Check for samples added and removed in msk bladder upload.
# Author: Haley Hunter-Zinck
# Date: 2022-03-16

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_upload <- "syn26250083"

# parameters
pt_rm <- c("GENIE-MSK-P-0021348")

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

data <- get_synapse_entity_data_in_csv(synid_file_upload)

# main ----------------------------

n_pt <- data %>%
  select(record_id) %>%
  distinct() %>%
  count()

data %>% 
  filter(is.element(record_id, pt_rm)) %>%
  select(record_id) %>%
  distinct()

# report out --------------

print(glue("Number of patients: {n_pt}"))


# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
