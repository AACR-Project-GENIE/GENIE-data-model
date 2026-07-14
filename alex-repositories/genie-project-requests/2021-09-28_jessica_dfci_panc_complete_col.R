# Description: 
# Author: Haley Hunter-Zinck
# Date: 

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_table_diag <- "syn21446701"
synid_dfci_panc_upload <- list(data1 = "syn25544638", header1 = "syn25544639")
synid_msk_panc_upload <- list(data1 = "syn25541828")
synid_file_panc_intake <- "syn24175803"
synid_file_panc_rcc <- "syn25578183"

# parameters
site_check = "DFCI"
cohort = "PANC"
site_control = "MSK"

# functions ----------------------------

get_synapse_entity_data_in_csv <- function(synapse_id, sep = ",", na.strings = c("NA")) {
  
  data <- read.csv(synGet(synapse_id)$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep)
  return(data)
}

get_synapse_entity_name <- function(synapse_id) {
  return(synGet(synapse_id, downloadFile = F)$properties$name)
}

# read ----------------------------

data_check <- get_synapse_entity_data_in_csv(synid_dfci_panc_upload$data1)
colnames(data_check) <- get_synapse_entity_data_in_csv(synid_dfci_panc_upload$header1)

data_control <- get_synapse_entity_data_in_csv(synid_msk_panc_upload$data1)

data_intake <- get_synapse_entity_data_in_csv(synid_file_panc_intake)

data_rcc <- data <- read.csv(synGet(synid_file_panc_rcc)$path, stringsAsFactors = F, 
                             na.strings = c("NA"), skip = 36, sep = "\t", check.names = T)
head(colnames(data_rcc))

# main ----------------------------

# check tables
query <- glue("SELECT cancer_diagnosis_complete,  redcap_data_access_group, COUNT(*) FROM {synid_table_diag} WHERE cohort = '{cohort}' GROUP BY cancer_diagnosis_complete,  redcap_data_access_group")
res <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))
print(res)

# check upload file
data_check %>% 
  filter(grepl(pattern = site_check, x = record_id)) %>%
  filter(redcap_repeat_instrument == "cancer_diagnosis") %>%
  select(cancer_diagnosis_complete) %>%
  group_by(cancer_diagnosis_complete) %>%
  count()

# check control upload file
data_control %>% 
  filter(grepl(pattern = site_control, x = record_id)) %>%
  filter(redcap_repeat_instrument == "cancer_diagnosis") %>%
  select(cancer_diagnosis_complete) %>%
  group_by(cancer_diagnosis_complete) %>%
  count()

# check intake for dfci
data_intake %>%
  filter(redcap_repeat_instrument == "Cancer Diagnosis") %>%
  filter(redcap_data_access_group == site_check) %>%
  select(cancer_diagnosis_complete) %>%
  group_by(cancer_diagnosis_complete) %>%
  count()
data_intake %>%
  filter(redcap_repeat_instrument == "Cancer Diagnosis") %>%
  filter(redcap_data_access_group == site_control) %>%
  select(cancer_diagnosis_complete) %>%
  group_by(cancer_diagnosis_complete) %>%
  count()

data_rcc %>% 
  filter(Event.Name.Occurrence. == "cancer_diagnosis") %>%
  filter(Site.Name == site_check) %>%
  select(cancer_diagnosis_complete) %>%
  group_by(cancer_diagnosis_complete) %>%
  count()

data_rcc %>% 
  filter(Event.Name.Occurrence. == "cancer_diagnosis") %>%
  filter(Site.Name == site_control) %>%
  select(cancer_diagnosis_complete) %>%
  group_by(cancer_diagnosis_complete) %>%
  count()


# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
