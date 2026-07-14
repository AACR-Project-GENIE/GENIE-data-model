# Description: Check VICC main GENIE invalid files.
# Author: Haley Hunter-Zinck
# Date: 2022-04-07

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(yaml)
library(synapser)

# synapse
synid_file_sam <- "syn7395712"
synid_file_inf <- "syn18134833"

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

#' Remove leading and trailing whitespace from a string.
#' @param str String
#' @return String without leading or trailing whitespace
trim <- function(str) {
  front <- gsub(pattern = "^[[:space:]]+", replacement = "", x = str)
  back <- gsub(pattern = "[[:space:]]+$", replacement = "", x = front)
  
  return(back)
}

# synapse login --------------------

status <- synLogin()

# read ----------------------------

sam <- get_synapse_entity_data_in_csv(synid_file_sam, sep = "\t")
inf <- read_yaml(synGet(synid_file_inf)$path)

# main ----------------------------

sam_seq_assay_id <- trim(as.character(unlist(sam %>% 
  select(SEQ_ASSAY_ID) %>%
  distinct())))

inf_seq_assay_id <- trim(names(inf))

setdiff(sam_seq_assay_id, inf_seq_assay_id)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
