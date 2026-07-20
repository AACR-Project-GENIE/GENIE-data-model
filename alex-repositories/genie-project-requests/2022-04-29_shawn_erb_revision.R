# Description: Explore ERBB2 upload data.
# Author: Haley Hunter-Zinck
# Date: 2022-04-29

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)

# synapse
synid_file_data <- "syn29875992"
synid_file_dd <- "syn29880018"
synid_table_map <- "syn8363731"

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

#' Read contents of an Excel Spreadsheet stored on Synapse.
#' 
#' @param synapse_id Synapse ID of the spreadsheet
#' @param version Version of the file
#' @param sheet Number of the sheet of the spreadsheet
#' @param check.names Whether R should modify names if non-conforming
#' @return Matrix of data
#' @example 
#' get_synapse_entity_data_in_xlsx(synapse_id = "syn123345", sheet = 2)
get_synapse_entity_data_in_xlsx <- function(synapse_id, 
                                            version = NA,
                                            sheet = 1,
                                            check.names = F) {
  library(openxlsx)
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.xlsx(entity$path, check.names = check.names, sheet = sheet)
  
  return(data)
}

format_var_value_names <- function(x) {
  var_names <- unique(unlist(lapply(strsplit(x = x, split = "___"), head, n = 1)))
  return(var_names)
}

# synapse login --------------------

synLogin()

# read ----------------------------

data <- get_synapse_entity_data_in_xlsx(synid_file_data)
dd <- get_synapse_entity_data_in_csv(synid_file_dd)

query <- glue("SELECT * FROM {synid_table_map}")
mapping <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))

# main ----------------------------

var_data <- format_var_value_names(colnames(data))
var_dd <- dd$`Variable / Field Name`

idx = which(mapping$code != mapping$cbio & !grepl(pattern = "_complete$", x = mapping$code))
var_map <- format_var_value_names(unlist(strsplit(x=mapping$code[idx], split = ',')))

var_data_not_dd <- setdiff(var_data, var_dd)
var_dd_not_data <- setdiff(var_dd, var_data)
var_map_not_dd <- setdiff(var_map, var_dd)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
