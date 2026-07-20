# Description: Check for non-numeric value in last MSK PANC upload in naaccr_histology_cd column
# Author: Haley Hunter-Zinck
# Date: 2021-10-04

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_panc_msk <- "syn25541828"
synid_table_ca_diag <- "syn21446701"

# functions ----------------------------

get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA,
                                           sep = ",", 
                                           na.strings = c("NA"), 
                                           header = T) {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = F,
                   header = header)
  return(data)
}


# read ----------------------------

data <- get_synapse_entity_data_in_csv(synid_file_panc_msk, na.strings = c(""))

# main ----------------------------

res_file <- data %>% 
  filter(grepl(pattern = "XX", x = data$naaccr_histology_cd)) %>%
  select(record_id, redcap_repeat_instrument, redcap_repeat_instance, naaccr_histology_cd)
print(res_file)

query <- glue("SELECT record_id, redcap_repeat_instance, naaccr_histology_cd FROM {synid_table_ca_diag} WHERE record_id = '{res_file$record_id}' AND redcap_repeat_instance = {res_file$redcap_repeat_instance}")
res_table <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))
print(res_table)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
