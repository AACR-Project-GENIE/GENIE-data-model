# Description: Determine why DFCI-PANC is missing information in the redcap_ca_seq and redcap_ca_index columns.
# Author: Haley Hunter-Zinck
# Date: 2021-11-10

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# parameters
DFCI = "DFCI"
cohort = "PANC"

# synapse
synid_table_upload <- "syn25892806"
synid_file_import <- "syn25472801"

# functions ----------------------------

read_bpc_upload <- function(cohort, site, synid_table_upload) {
  
  synid_file_upload <- c()
  
  if(site == DFCI) {
    
    # read data part 1
    query <- glue("SELECT data1 FROM {synid_table_upload} WHERE cohort = '{cohort}' AND site = '{site}'")
    ent_id <- synTableQuery(query, includeRowIdAndRowVersion=FALSE)
    synid_file_upload = append(synid_file_upload, as.character(read.csv(ent_id$filepath, na.strings = c(""), stringsAsFactors = FALSE)))
    ent_data <- synGet(tail(synid_file_upload,1))
    data1 <- read.csv(ent_data$path, check.names = FALSE, na.strings = c(""), stringsAsFactors = FALSE)
    
    # read associated header, part 1
    query <- glue("SELECT header1 FROM {synid_table_upload} WHERE cohort = '{cohort}' AND site = '{site}'")
    ent_id <- synTableQuery(query, includeRowIdAndRowVersion=FALSE)
    synid_file_upload = append(synid_file_upload, as.character(read.csv(ent_id$filepath, na.strings = c(""), stringsAsFactors = FALSE)))
    ent_data <- synGet(tail(synid_file_upload,1))
    colnames(data1) <- as.character(read.csv(ent_data$path, check.names = FALSE, na.strings = c(""), stringsAsFactors = FALSE))
    
    # attempt to read data part 2
    query <- glue("SELECT data2 FROM {synid_table_upload} WHERE cohort = '{cohort}' AND site = '{site}'")
    ent_id <- synTableQuery(query, includeRowIdAndRowVersion=FALSE)
    synid_tmp <- as.character(unlist(read.csv(ent_id$filepath, stringsAsFactors = FALSE)))
    
    if(length(synid_tmp) > 0 && !is.na(synid_tmp)) {
      synid_file_upload = append(synid_file_upload, as.character(read.csv(ent_id$filepath, na.strings = c(""), stringsAsFactors = FALSE)))
      ent_data <- synGet(tail(synid_file_upload,1))
      data2 <- read.csv(ent_data$path, check.names = FALSE, na.strings = c(""), stringsAsFactors = FALSE)
      
      # read associated header, part 2
      query <- glue("SELECT header2 FROM {synid_table_upload} WHERE cohort = '{cohort}' AND site = '{site}'")
      ent_id <- synTableQuery(query, includeRowIdAndRowVersion=FALSE)
      synid_file_upload = append(synid_file_upload, as.character(read.csv(ent_id$filepath, na.strings = c(""), stringsAsFactors = FALSE)))
      ent_data <- synGet(tail(synid_file_upload,1))
      colnames(data2) <- as.character(read.csv(ent_data$path, check.names = FALSE, na.strings = c(""), stringsAsFactors = FALSE))
      
      # merge
      data <- merge(data1, data2, on ='record_id')
    } else {
      data <- data1
    }
  } else {
    
    # read data with header for all other sites
    query <- glue("SELECT data1 FROM {synid_table_upload} WHERE cohort = '{cohort}' AND site = '{site}'")
    ent_id <- synTableQuery(query, includeRowIdAndRowVersion=FALSE)
    synid_file_upload <- as.character(read.csv(ent_id$filepath, na.strings = c(""), stringsAsFactors = FALSE))
    ent_data <- synGet(synid_file_upload)
    data <- read.csv(ent_data$path, check.names = FALSE, na.strings = c(""), stringsAsFactors = FALSE)
    
  } 
  
  return(data)
}

# read ----------------------------

data_msk_panc <- read_bpc_upload(cohort = "PANC", site = "MSK", synid_table_upload)
data_dfci_panc <- read_bpc_upload(cohort = "PANC", site = "DFCI", synid_table_upload)
data_dfci_panc_v1 <- read.csv(synGet("syn25544638", version = 1)$path)
header <- as.character(unlist(read.csv(synGet("syn25544639", version = 2)$path)))
colnames(data_dfci_panc_v1) <- header
data_dfci_prostate <- read_bpc_upload(cohort = "Prostate", site = "DFCI", synid_table_upload)
import <- as.character(unlist(data.frame(scan(synGet(synid_file_import)$path, what = "character", sep = ","))))

setdiff(colnames(data_dfci_panc), import)
setdiff(import, colnames(data_dfci_panc))

setdiff(colnames(data_dfci_prostate), import)
setdiff(import, colnames(data_dfci_prostate))

data_dfci_prostate %>% filter(redcap_repeat_instrument == 'cancer_diagnosis' & is.na(redcap_ca_index)) %>% count()
data_dfci_prostate %>% filter(redcap_repeat_instrument == 'cancer_diagnosis' & is.na(redcap_ca_seq)) %>% count()
data_msk_panc %>% filter(redcap_repeat_instrument == 'cancer_diagnosis' & is.na(redcap_ca_index)) %>% count()
data_msk_panc %>% filter(redcap_repeat_instrument == 'cancer_diagnosis' & is.na(redcap_ca_seq)) %>% count()
data_dfci_panc %>% filter(redcap_repeat_instrument == 'cancer_diagnosis' & is.na(redcap_ca_index)) %>% count()
data_dfci_panc %>% filter(redcap_repeat_instrument == 'cancer_diagnosis' & is.na(redcap_ca_seq)) %>% count()
data_dfci_panc_v1 %>% filter(redcap_repeat_instrument == 'cancer_diagnosis' & is.na(redcap_ca_index)) %>% count()
data_dfci_panc_v1 %>% filter(redcap_repeat_instrument == 'cancer_diagnosis' & is.na(redcap_ca_seq)) %>% count()

# main ----------------------------


# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
