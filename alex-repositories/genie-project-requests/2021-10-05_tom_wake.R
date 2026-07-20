# Description: Prove that sample IDs do not match.
# Author: Haley Hunter-Zinck
# Date: 2021-10-05

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_wake_cli <- "syn23679302"
synid_file_wake_mut <- "syn14732172"
version_curr_cli <- 8
version_prev_cli <- 7
version_curr_mut <- 22
version_prev_mut <- 11

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

data_prev_cli <- get_synapse_entity_data_in_csv(synid_file_wake_cli, sep = "\t", version = version_prev_cli)
data_curr_cli <- get_synapse_entity_data_in_csv(synid_file_wake_cli, sep = "\t", version = version_curr_cli)
data_prev_mut <- get_synapse_entity_data_in_csv(synid_file_wake_mut, sep = "\t", version = version_prev_mut)
data_curr_mut <- get_synapse_entity_data_in_csv(synid_file_wake_mut, sep = "\t", version = version_curr_mut)


# main ----------------------------

sam_prev_cli <- data_prev_cli$SAMPLE_ID
sam_curr_cli <- data_curr_cli$SAMPLE_ID
sam_prev_mut <- unique(data_prev_mut$Tumor_Sample_Barcode)
sam_curr_mut <- unique(data_curr_mut$Tumor_Sample_Barcode)

length(intersect(sam_prev_cli, sam_prev_mut))
length(intersect(sam_prev_cli, sam_curr_mut))
length(intersect(sam_curr_cli, sam_curr_mut))

length(sam_prev_cli)
length(sam_curr_cli)



# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
