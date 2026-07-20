# Description: Investigate uplaods and tables for tumor marker data for selected samples.
# Author: Haley Hunter-Zinck
# Date: 2021-11-29

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_table_tm <- "syn21446708"
synid_file_dfci_data <- "syn26230815"
synid_file_dfci_header <- "syn25592217"
version_file_dfci_data <- 3
version_file_dfci_header <- 1

# parameters
record_ids <- c("GENIE-DFCI-008892", "GENIE-DFCI-008986", "GENIE-DFCI-009276", "GENIE-DFCI-009931")

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

header <- get_synapse_entity_data_in_csv(synapse_id = synid_file_dfci_header,
                                       version = version_file_dfci_header)
data <- get_synapse_entity_data_in_csv(synapse_id = synid_file_dfci_data,
                                       version = version_file_dfci_data)
names(data) <- header

# main ----------------------------

# check table
record_id_str <- paste0("'", paste0(record_ids, collapse = "','"), "'")
query <- glue("SELECT COUNT(*) FROM {synid_table_tm} WHERE record_id IN ({record_id_str})")
n_tbl_subset <- unlist(as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = T)))

# check upload
n_tm_all <- data %>% 
  filter(redcap_repeat_instrument == 'prissmm_tumor_marker') %>%
  select(record_id) %>% 
  distinct() %>% 
  count()
n_tm_subset <- data %>% 
  filter(redcap_repeat_instrument == 'prissmm_tumor_marker' & is.element(record_id, record_ids)) %>%
  select(record_id) %>% 
  distinct()  %>% 
  count()

# close out ----------------------------

print(glue("Record IDS of interest: {record_id_str}"))
print(glue("Record IDs of interest found in tumor marker table: {n_tbl_subset}"))
print(glue("Number of records with tumor marker instrument found in data file: {n_tm_all}"))
print(glue("Record IDs of interest with tumor marker instrument found in data file: {n_tm_subset}"))

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
