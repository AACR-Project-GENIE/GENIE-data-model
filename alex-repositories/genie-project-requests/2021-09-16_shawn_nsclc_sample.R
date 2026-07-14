# Description: Sample 50 patients from the BPC NSCLC 2.1 consortium release.  
# Author: Haley Hunter-Zinck
# Date: 2021-09-16
# Request:
#     Source: Shawn Sweeney
#     Request date: 2021-09-16
#     Message:  randomly select 50 patients from the BPC NSCLC 2.1-consortium 
#              release and generate the 8 release files for just those 50 patients
# Note on usage: remember to do "Session --> Restart R" before running so same 
#     patient IDs sampled from the stored random seed

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(synapser)
synLogin()
library(dplyr)

# synapse
synid_folder_sample <- "syn26209100"
synid_folder_data <- "syn25982471"

# parameters
n_sample = 50

# random
seed <- 540827
set.seed(seed)

# functions ----------------------------

get_synapse_entity_name <- function(synapse_id) {
  return(synGet(synapse_id, downloadFile = F)$properties$name)
}

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

get_synapse_entity_data_in_csv <- function(synapse_id, sep = ",", na.strings = c("NA"), header = T) {
  
  data <- read.csv(synGet(synapse_id)$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, header = header)
  return(data)
}

get_synapse_folder_children <- function(synapse_id) {
  
  
  ent <- as.list(synGetChildren(synapse_id))
  
  children <- c()
  for (i in 1:length(ent)) {
    children[ent[[i]]$name] <- ent[[i]]$id
  }
  
  return(children)
}

# read ----------------------------

synid_files_data <- get_synapse_folder_children(synid_folder_data)

datas <- list()
for (i in 1:length(synid_files_data)) {
  synapse_id = as.character(synid_files_data[i])
  datas[[synapse_id]] <- get_synapse_entity_data_in_csv(synapse_id)
}

# main ----------------------------

# sample 50 IDs randomly for patient level file
synid_file_patient <- as.character(synid_files_data["patient_level_dataset.csv"])
patient_ids <- unlist(sample(datas[[synid_file_patient]]$record_id, n_sample))

sampled <- list()
for (i in 1:length(datas)) {
  sampled[[names(datas)[i]]] <- datas[[names(datas)[i]]] %>% 
    filter(is.element(record_id, patient_ids))
}

# checks --------------------------------

print(glue("Number of samples in patient list equals number of requested samples: {n_sample == length(patient_ids)}"))

# record_ids
for (i in 1:length(sampled)) {
  
  synapse_id <- names(sampled)[i]
  file_name <- get_synapse_entity_name(synapse_id)
  
  print(glue("File: {file_name} ({synapse_id})"))
  print(glue("Dimensions of the extract: {nrow(sampled[[i]])} x {ncol(sampled[[i]])}"))
  print(glue("All record IDs are in the manifest: {all(is.element(unlist(sampled[[i]]$record_id), patient_ids))}"))
  print(glue("Number of overlapping records IDs: {length(intersect(unlist(sampled[[i]]$record_id), patient_ids))}"))
  print(glue("Number of missing records IDs: {length(setdiff(patient_ids, unlist(sampled[[i]]$record_id)))}"))
  print(glue("Number of columns: {ncol(sampled[[i]])}"))
  print(glue("Number of columns in sampled equals that in original data frame: {ncol(sampled[[i]]) == ncol(datas[[i]])}"))
  print("-----------")
}

# write -----------------------------

# # patient IDs to synapse
file_output <- "patient_ids_sample.csv"
write(patient_ids, file = file_output, ncolumns = 1)
save_to_synapse(path = file_output,
                parent_id = synid_folder_sample,
                prov_name = "Patient IDs for data sample",
                prov_desc = "Fifty patient IDs sampled from the GENIE BPC NSCLC 2.1-consortium release",
                prov_used = synid_file_patient,
                prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2021-09-16_shawn_nsclc_sample.R")
file.remove(file_output)

# data sample files to synapse
for (i in 1:length(sampled)) {

  file_output <- gsub(pattern = ".csv", replacement = "_sample.csv",
                    x = get_synapse_entity_name(names(sampled)[i]))

  write.csv(sampled[[i]], row.names = F, file = file_output)
  save_to_synapse(path = file_output,
                  parent_id = synid_folder_sample,
                  prov_name = "Data sample",
                  prov_desc = "Data for fifty sampled patients from the GENIE BPC NSCLC 2.1-consortium release",
                  prov_used = names(sampled)[i],
                  prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2021-09-16_shawn_nsclc_sample.R")
  file.remove(file_output)
}

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
