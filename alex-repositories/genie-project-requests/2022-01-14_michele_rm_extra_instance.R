# Description: Remove extra blank instance from VICC BLADDER upload.
# Author: Haley Hunter-Zinck
# Date: 2022-01-14

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_vicc <- "syn26250085"
synid_folder_output <- "syn26243286"

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

#' Store a file on Synapse with options to define provenance.
#' 
#' @param path Path to the file on the local machine.
#' @param parent_id Synapse ID of the folder or project to which to load the file.
#' @param file_name Name of the Synapse entity once loaded
#' @param prov_name Provenance short description title
#' @param prov_desc Provenance long description
#' @param prov_used Vector of Synapse IDs of data used to create the current
#' file to be loaded.
#' @param prov_exec String representing URL to script used to create the file.
#' @return Synapse ID of entity representing file
save_to_synapse <- function(path, 
                            parent_id, 
                            file_name = NA, 
                            prov_name = NA, 
                            prov_desc = NA, 
                            prov_used = NA, 
                            prov_exec = NA) {
  
  if (is.na(file_name)) {
    file_name = path
  } 
  file <- File(path = path, parentId = parent_id, name = file_name)
  
  if (!is.na(prov_name) || !is.na(prov_desc) || !is.na(prov_used) || !is.na(prov_exec)) {
    act <- Activity(name = prov_name,
                    description = prov_desc,
                    used = prov_used,
                    executed = prov_exec)
    file <- synStore(file, activity = act)
  } else {
    file <- synStore(file)
  }
  
  return(file$properties$id)
}

get_synapse_entity_name <- function(synapse_id) {
  return(synGet(synapse_id, downloadFile = F)$properties$name)
}


# read ----------------------------

data <- as.matrix(get_synapse_entity_data_in_csv(synid_file_vicc, na.strings = ""))


# main ----------------------------

idx <- which(data[,"record_id"] == "GENIE-VICC-947519" & 
               data[,"redcap_repeat_instrument"] == "prissmm_tumor_marker" & 
               data[, "redcap_repeat_instance"] == " 1")
subset <- data[-idx,]

file_local <- "vicc_bladder_fix.csv"
write.csv(subset, row.names = F, file = file_local, na = "")
save_to_synapse(path = file_local, 
                parent_id = synid_folder_output, 
                file_name = get_synapse_entity_name(synid_file_vicc),
                prov_name = "remove empty instance", 
                prov_desc = "remove empty instance in latest VICC bladder upload", 
                prov_used = synid_file_vicc, 
                prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2022-01-14_michele_rm_extra_instance.R")


# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
