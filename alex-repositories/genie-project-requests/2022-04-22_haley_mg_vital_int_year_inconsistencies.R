# Description: Determine frequency of vital status INT and YEAR inconsistencies in the main
#       GENIE 11.6-consortium release.
# Author: Haley Hunter-Zinck
# Date: 2022-04-22

# setup ----------------------------

tic = as.double(Sys.time())

library(rmarkdown)
library(glue)
library(synapser)
synLogin()

# file prefix
prefix <- "2022-04-22_haley_mg_vital_int_year_inconsistencies"

# synapse
synid_file_patient <- "syn9734568"
synid_version_patient <- 189
synid_folder_output <- "syn26145645"

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

# main ----------------------------

rmarkdown::render(glue("{prefix}.Rmd"), param = list(synid_file_patient = synid_file_patient, synid_version_patient = synid_version_patient))

synid_file_output <- save_to_synapse(path = glue("{prefix}.html"), 
                                      parent_id = synid_folder_output,
                                      prov_name = "main genie vital info", 
                                      prov_desc = "summary of main genie vital INT and YEAR inconsistencies", 
                                      prov_used = synid_file_patient, 
                                      prov_exec = glue("https://github.com/Sage-Bionetworks/genie-project-requests/blob/main/{prefix}.Rmd"))

file.remove(glue("{prefix}.html"))

# close out -----------------------

print(glue("Report stored at {synid_file_output}"))

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
