# Description: Investigate duplicated variants in release 12.1-consortium.
# Author: Haley Hunter-Zinck
# Date: 2022-01-24

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_uchi_flat <- "syn22275379"
synid_ver_uchi_flat <- "24"
synid_folder_output <- "syn12278118"

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

#' Get the name of a Synapse entity. 
#' 
#' @param synapse_id Synapse ID string
#' @return String representing entity name
#' @example get_synapse_entity_name("syn12345")
get_synapse_entity_name <- function(synapse_id) {
  return(synGet(synapse_id, downloadFile = F)$properties$name)
}

# read ----------------------------

uchi_flat <- get_synapse_entity_data_in_csv(synid_file_uchi_flat, version = synid_ver_uchi_flat, sep = "\t")

# main ----------------------------

ind_dup_uchi_flat <- uchi_flat %>% 
  select(Chromosome, Start_Position, Reference_Allele, Tumor_Seq_Allele2, Tumor_Sample_Barcode) %>%
  duplicated()
print(glue("Number of duplicated variants in uchi flat file ({synid_file_uchi_flat}.{synid_ver_uchi_flat}): {sum(ind_dup_uchi_flat)}"))

uchi_dedup <- uchi_flat %>%
  filter(!ind_dup_uchi_flat)

print(dim(uchi_flat))
print(dim(uchi_dedup))

print(nrow(uchi_flat) - nrow(uchi_dedup) == sum(ind_dup_uchi_flat))

# write ---------------------------------

write.csv(uchi_dedup, sep = "\t", row.names = F)
save_to_synapse(path, 
                parent_id = synid_folder_output, 
                file_name = get_entity_name(synid_file_uchi_flat), 
                prov_name = "de-duplication", 
                prov_desc = "remove duplicated variants from center flat file", 
                prov_used = synid_file_uchi_flat, 
                prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2022-01-25_tom_remove_duplicates.R")

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
