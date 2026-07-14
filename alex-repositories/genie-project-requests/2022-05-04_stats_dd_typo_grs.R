# Description: Check if variables with a typo exist in the global response set.
# Author: Haley Hunter-Zinck
# Date: 2022-05-04

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)

# synapse
synid_file_grs <- "syn24184523"

# parameters
vars <- c("^ca_type$", "^path_ca_type[0-9]+", "^md_type_ca_cur$")
text_old <- "Adrenocorical"
text_new <- "Adrenocortical"

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

grs <- get_synapse_entity_data_in_csv(synid_file_grs)

# main ----------------------------

for (var in vars) {
  
  idx <- grep(x = grs$`Response Set Label`, pattern = var)
  n_contain_typo <- length(which(grepl(x = grs[idx,]$`Response Set Values`, pattern = text_old)))
  
  print(glue("Variable regex: '{var}'"))
  print(glue("- Number of variable name matches: {length(idx)}"))
  print(glue("- Number with typo in choices: {n_contain_typo}"))
  
}

n_total_var <- nrow(grs)
n_total_contain_typo <- length(which(grepl(x = grs$`Response Set Values`, pattern = text_old)))

print(glue("- Number of variables: {n_total_var}"))
print(glue("- Number with typo in choices: {n_total_contain_typo}"))

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
