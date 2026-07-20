# Description: Get TMB estimates for a sample of main GENIE samples based on the 11.1-public release.
# Author: Haley Hunter-Zinck
# Date: 2022-04-27

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)

# synapse
synid_file_tmb <- "syn29818001"
synid_file_ids <- "syn29817969"
synid_folder_output <- "syn29817911"

# parameters
file_output <- "tmb_subset.csv"

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
#' @param comment.char character designating comment lines to ignore
#' @return data frame
get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA,
                                           sep = ",", 
                                           na.strings = c("NA"), 
                                           header = T,
                                           check_names = F,
                                           comment.char = "#",
                                           colClasses = "character") {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = check_names,
                   header = header, comment.char = comment.char, colClasses = colClasses)
  return(data)
}

#' Read contents of an Excel Spreadsheet stored on Synapse.
#' 
#' @param synapse_id Synapse ID of the spreadsheet
#' @param version Version of the file
#' @param sheet Number of the sheet of the spreadsheet
#' @param check.names Whether R should modify names if non-conforming
#' @return Matrix of data
#' @example 
#' get_synapse_entity_data_in_xlsx(synapse_id = "syn123345", sheet = 2)
get_synapse_entity_data_in_xlsx <- function(synapse_id, 
                                            version = NA,
                                            sheet = 1,
                                            check.names = F) {
  library(openxlsx)
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.xlsx(entity$path, check.names = check.names, sheet = sheet)
  
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

# synapse login --------------------

synLogin()

# read ----------------------------

ref <- get_synapse_entity_data_in_xlsx(synid_file_ids)
tmb <- get_synapse_entity_data_in_csv(synid_file_tmb, sep = "\t")

# main ----------------------------

id_sam <- ref$Sample.ID
tmb_subset <- tmb %>%
  filter(is.element(SAMPLE_ID, id_sam))


write.csv(tmb_subset, file = file_output, row.names = F)
save_to_synapse(path = file_output, 
                            parent_id = synid_folder_output, 
                            prov_name = "TMB estimate for subset", 
                            prov_desc = "Tumor mutation burden (TMB) estimates for a subset of main GENIE samples", 
                            prov_used = c(synid_file_ids, synid_file_tmb), 
                            prov_exec = "https://github.com/Sage-Bionetworks/genie-project-requests/blob/main/2022-04-27_shawn_tmb.R")
file.remove(file_output)

# close out ----------------------------

print(glue("Number of sample IDs in reference: {length(id_sam)}"))
print(glue("Number of unique sample IDs in reference: {length(unique(id_sam))}"))
print(glue("Number of sample IDs in TMB subset: {length(tmb_subset$SAMPLE_ID)}"))
print(glue("IDs not in TMB subset: {paste0(setdiff(id_sam,tmb_subset$SAMPLE_ID), collapse = ', ')}"))

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
