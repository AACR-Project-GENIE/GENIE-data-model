# Description: Get details on MG file errors.
# Author: Haley Hunter-Zinck
# Date: 2022-04-012

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)

# synapse
synid_file_cli <- "syn17023617"

# parameters
valid_seq_dates <- c("Jan", "Apr", "Jul", "Oct")

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

data <- get_synapse_entity_data_in_csv(synid_file_cli, sep = "\t")

# main ----------------------------

# invalid SEQ_DATE
n_invalid_month <- data %>% 
  filter(!is.element(substr(SEQ_DATE, 1, 3), valid_seq_dates)) %>%
  count()
n_year_four <- data %>%
  filter(grepl(pattern = "[A-z]{3}-[0-9]{4}", x = SEQ_DATE)) %>%
  count()
n_year_two <- data %>%
  filter(grepl(pattern = "[A-z]{3}-[0-9]{2}", x = SEQ_DATE)) %>%
  count()

# invalid oncotree code
record_invalid_oncotree <- unlist(data %>% 
  filter(ONCOTREE_CODE == "") %>%
  select(PATIENT_ID))

# report -----------------------------

print(glue("Number of invalid months in SEQ_DATE: {n_invalid_month}"))
print(glue("Number of four digit (valid) years in SEQ_DATE: {n_year_four}"))
print(glue("Number of two digit (invalid) years in SEQ_DATE: {n_year_two}"))
print(glue("Record(s) with blank (invalid) OncoTree codes: {paste0(record_invalid_oncotree, collapse = ',')}"))

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
