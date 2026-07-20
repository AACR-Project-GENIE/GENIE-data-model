# Description: Generate sample list of clinical sample files.  
# Author: Haley Hunter-Zinck
# Date: 2021-11-19

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_cpt <- "syn26213917"
synid_folder_output <- "syn26470934"
synid_file_pt <- "syn26213898"

# files
file_sample_output <- "sample_ids_sample.csv"
file_patient_output <- "patient_ids_sample.csv"

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

extract_patient_id <- function(x) {
  
  splt <- strsplit(x, split = "-")[[1]]
  
  if (grepl(pattern = "MSK", x = x)) {
    return(glue("{splt[1]}-{splt[2]}-{splt[3]}-{splt[4]}"))
  } 
  
  return(glue("{splt[1]}-{splt[2]}-{splt[3]}"))
}

# main ----------------------------

data <- get_synapse_entity_data_in_csv(synid_file_cpt)
sample_ids <- data$cpt_genie_sample_id

patient_ids <- unlist(get_synapse_entity_data_in_csv(synid_file_pt, header = F))

# check -----------------------

patient_ids_from_sample_ids <- unique(unlist(lapply(as.list(sample_ids), extract_patient_id)))

setdiff(patient_ids, patient_ids_from_sample_ids)
setdiff(patient_ids_from_sample_ids, patient_ids)
length(patient_ids)
length(patient_ids_from_sample_ids)
length(intersect(patient_ids_from_sample_ids, patient_ids))

# write ----------------------

write(sample_ids, ncolumn = 1, file = file_sample_output)
write(patient_ids, ncolumn = 1, file = file_patient_output)

save_to_synapse(path = file_sample_output, 
                parent_id = synid_folder_output, 
                prov_name = "sample list", 
                prov_desc = "sample list derived from cancer panel test file", 
                prov_used = synid_file_cpt, 
                prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2021-11-19_ritika_nsclc_sample_list.R")
save_to_synapse(path = file_patient_output, 
                parent_id = synid_folder_output, 
                prov_name = "patient list", 
                prov_desc = "patient list copied", 
                prov_used = synid_file_pt, 
                prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2021-11-19_ritika_nsclc_sample_list.R")

file.remove(file_sample_output)
file.remove(file_patient_output)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
