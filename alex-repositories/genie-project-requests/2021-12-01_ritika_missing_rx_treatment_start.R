# Description: examine missing start dates for radiation treatment in cBioPortal files.
# Author: Haley Hunter-Zinck
# Date: 2021-12-01

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_cbio <- "syn26349077"
synid_file_der_radtx <- "syn25931923"
synid_table_rx <- "syn21446710"
synid_table_dx <- "syn21446701"

# parameters
cohort = "PANC"
patient_ids <- c("GENIE-DFCI-040391", "GENIE-DFCI-109323")

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

data_der <- get_synapse_entity_data_in_csv(synid_file_der_radtx, sep = ",")
data_cbio <- get_synapse_entity_data_in_csv(synid_file_cbio, sep = "\t")

query <- glue("SELECT record_id, rt_start_time, rt_stop_time, rt_dose, rt_fractions, rt_total_dose, rt_start_int, rt_end_int FROM {synid_table_rx} WHERE cohort = '{cohort}'")
data_tbl <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))

str_ids <- paste0("'", paste0(patient_ids, collapse = "','"), "'")
query <- glue("SELECT cohort, record_id, ca_cadx_int FROM {synid_table_dx} WHERE cohort = '{cohort}' AND record_id IN ({str_ids})")
data_dx <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))

# main ----------------------------

print("Table data:")
print(data_tbl %>%
  filter(is.element(record_id, patient_ids)))
print(data_dx)
print("--------")

print("Derived radiation treatment variable file: ")
print(data_der %>%
  filter(is.element(record_id, patient_ids)) %>%
  select(record_id, rt_start_time, rt_stop_time, rt_dose, rt_fractions, rt_total_dose, dx_rt_start_days, rt_start_int))
print("--------")

print("cBioPortal file:")
print(data_cbio %>%
  filter(is.element(PATIENT_ID, patient_ids) & !is.na(RT_TOTAL_DOSE)) %>%
  select(PATIENT_ID, START_DATE, STOP_DATE, RT_DOSE, RT_FRACTIONS, RT_TOTAL_DOSE))
print("--------")

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
