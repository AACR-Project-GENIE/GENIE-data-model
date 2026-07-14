# Description: Move a UHN upload to correct Synapse entity.
# Author: Haley Hunter-Zinck
# Date: 2021-10-25
# Request: 
#   source: haley after email from Mike giving link to upload
#   date requested: 2021-10-25

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_source <- "syn26401279"
synid_folder_dest <- "syn26243339"
synid_file_dest <- "syn26212729"

# functions ----------------------------

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
#' @return TRUE if successful, otherwise return error.
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
  
  return(T)
}

#' Get the name of a Synapse entity. 
#' 
#' @param synapse_id Synapse ID string
#' @return String representing entity name
#' @example get_synapse_entity_name("syn12345")
get_synapse_entity_name <- function(synapse_id) {
  return(synGet(synapse_id, downloadFile = F)$properties$name)
}

# main ----------------------------

entity <- synGet(synid_file_source)
file_name <- get_synapse_entity_name(synid_file_dest)
save_to_synapse(path = entity$path, 
                parent_id = synid_folder_dest, 
                file_name = file_name, 
                prov_name = "copy file", 
                prov_desc = "copy file from upload to sponsor delivery folder", 
                prov_used = synid_file_source, 
                prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2021-10-26_uhn_move_file_upload.R")

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
