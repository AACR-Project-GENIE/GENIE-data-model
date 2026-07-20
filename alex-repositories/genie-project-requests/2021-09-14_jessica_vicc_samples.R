# Description: Check whether samples are present in VICC upload file
# Author: Haley Hunter-Zinck
# Date: September 14, 2021
# Request: 
#     Source: email from Jessica Lavery 
#     Date requested: 2021-09-14
#     Msg: Can you please confirm that the two VICC cases (GENIE-VICC-342704, GENIE-VICC-747689) 
#          were excluded from the VICC data upload and not dropped during creation of the 
#          synapse tables?

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(synapser)
synLogin()
library(dplyr)

# synapse
synid_file_vicc_panc_upload <- "syn25541702"

# constnat
patient_ids_check <- c("GENIE-VICC-342704", "GENIE-VICC-747689") 
n_patient_control <- 10

# functions ----------------------------

get_synapse_entity_data_in_csv <- function(synapse_id, sep = ",", na.strings = c("NA")) {
  
  data <- read.csv(synGet(synapse_id)$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep)
  return(data)
}

# read ----------------------------

data <- get_synapse_entity_data_in_csv(synapse_id = synid_file_vicc_panc_upload)

# main ----------------------------

# check for ids
n_check <- data %>% 
  filter(is.element(record_id, patient_ids_check)) %>%
  select(record_id) %>%
  count()

# ensure that check is valid
n_control <- data %>% 
  filter(is.element(record_id, head(unique(data$record_id), n = n_patient_control))) %>%
  select(record_id) %>%
  distinct() %>%
  count()

# close out ----------------------------

print(glue("Number of IDs to check found: {n_check}"))
print(glue("Number of IDs in control found (should be {n_patient_control}): {n_control}"))

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
