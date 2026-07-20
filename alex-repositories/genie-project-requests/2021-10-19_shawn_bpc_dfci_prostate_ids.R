# Description: 
# Author: Haley Hunter-Zinck
# Date: 2021-10-19
# Request: 

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(synapser)
synLogin()

# synapse
synid_table_pt <- "syn21446700"
synid_folder_store <- "syn26162727"
cohort <- "Prostate"
site <- "DFCI"

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

# main ----------------------------

query <- glue("SELECT DISTINCT record_id FROM {synid_table_pt} WHERE cohort = '{cohort}' AND redcap_data_access_group = '{site}'")
ids <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = T))

# write --------------------------------

file_output <- glue("2021-10-19_{tolower(cohort)}_{tolower(site)}_record_ids.csv")
write.csv(ids, row.names = F, file = file_output)

save_to_synapse(path = file_output, 
                parent_id = synid_folder_store, 
                prov_name = "dfci prostate ids", 
                prov_desc = "Extract current list of records IDs for the BPC Prostate cohort from DFCI", 
                prov_used = synid_table_pt, 
                prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2021-10-19_shawn_bpc_dfci_prostate_ids.R")

file.remove(file_output)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
