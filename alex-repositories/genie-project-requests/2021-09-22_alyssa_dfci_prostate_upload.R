# Description: Determine why 2021-09-21 DFCI prostate upload is missing sample IDs.  
# Author: Haley Hunter-Zinck
# Date: 2021-09-22
# Request:
#   Source: slack message from Alyssa highlighting the issue in the QA reports
#   Message: I am having trouble shooting the issues found in https://www.synapse.org/#!Synapse:syn25931845 
#            as it seems that the records for the missing sample IDs do have sample IDs

# user input ----------------------------

cohort = "Prostate"
site = "DFCI"

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(data.table)
library(synapser)
synLogin()

# synapse
synid_table_upload <- "syn25892806"
synid_table_genie_sample <- "syn7517674"

# storage
synid_file_upload <- c()

# functions ----------------------------

# read ----------------------------

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

# set up data matrix
data <- data1
setDT(data)

# main ----------------------------

# Number of Records
print(paste("Number of unique patient IDs for", site, ": ", length(unique(data$record_id)) ))

# Check if IDs match Main Genie list --- Sample IDs
sample_id_info <- data[redcap_repeat_instrument=='cancer_panel_test',list(record_id, redcap_repeat_instance, cpt_genie_sample_id)]
sample_id_list <- sample_id_info$cpt_genie_sample_id
temp <- toString(sample_id_list)
temp <- sapply(strsplit(temp, '[, ]+'), function(x) toString(shQuote(x)))
sample_in_genie <-synTableQuery(glue("select SAMPLE_ID from {synid_table_genie_sample} 
                                       where SAMPLE_ID in ({temp})"))
sample_in_genie <- sample_in_genie$asDataFrame()$SAMPLE_ID
sample_not_in_genie <- sample_id_info[!sample_id_info$cpt_genie_sample_id %in% sample_in_genie,]

# look at sample info for records missing sample IDs
res<- sample_id_info[is.element(record_id, sample_not_in_genie$record_id),]

# summary ------------------------------

n_na_repeat_1 <- length(which(res$redcap_repeat_instance[is.na(res$cpt_genie_sample_id)] == 1))
n_na_repeat_2_plus <- length(which(res$redcap_repeat_instance[is.na(res$cpt_genie_sample_id)] > 1))
print(glue("Number of repeat instance 1 that have sample ID as NA: {n_na_repeat_1}"))
print(glue("Number of repeat instance 2+ that have sample ID as NA: {n_na_repeat_2_plus}"))
table(res$redcap_repeat_instance[is.na(res$cpt_genie_sample_id)])

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
