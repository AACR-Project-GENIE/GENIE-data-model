# Description: explore issues with cBioPortal files for PANC cohort
# Author: Haley Hunter-Zinck
# Date: 2022-02-14

# setup ----------------------------

rm(list = ls())
tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
#synid_file_survival <- "syn26349089" # PANC
#synid_file_survival <- "syn26475431" # Prostate
synid_file_survival <- "syn26990173" # BLADDER

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
                                           comment.char = "#") {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = check_names,
                   header = header, comment.char = comment.char)
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

# read ----------------------------

survival <- get_synapse_entity_data_in_csv(synid_file_survival, sep = "\t")

# main ----------------------------

pt <- unlist(survival %>% 
  select(PATIENT_ID) %>%
  distinct())
n_unique_pt <- length(pt)

idx_rm <- c()
for (i in 1:length(pt)) {
  idx <- which(survival[,"PATIENT_ID"] == pt[i])
  if (length(idx) > 1) {
    idx_rm <- c(idx_rm, which(survival[,"PATIENT_ID"] == pt[i] & (is.na(survival[,"PFS_I_ADV_STATUS"]) | survival[,"PFS_I_ADV_STATUS"] == "")))
  }
}

if (length(idx_rm)) {
  fil <- survival[-idx_rm,]
} else {
  fil <- survival
}

n_fil_distinct <- fil %>% 
  select(PATIENT_ID) %>%
  distinct() %>%
  count()
n_fil_count <- fil %>% 
  count()

print(n_fil_distinct == n_fil_count)
print(glue("n_unique_pt = {n_unique_pt}"))
print(glue("n_fil_distinct = {n_fil_distinct}"))
print(glue("n_fil_count = {n_fil_count}"))
print(head(survival))
print(head(fil))

# alternate ----------------------------

idx_na <- which(is.na(survival[,"PFS_I_ADV_STATUS"]) | survival[,"PFS_I_ADV_STATUS"] == "")
idx_dup <- which(duplicated(survival[,"PATIENT_ID"], fromLast = F) |
                 duplicated(survival[,"PATIENT_ID"], fromLast = T))
idx_rm <- intersect(idx_dup, idx_na)
if (length(idx_rm)) {
  fil_alt <- survival[-idx_rm,]
} else {
  fil_alt <- survival
}

print("Percentage of patients in newly filtered df: ")
print(length(intersect(pt, fil_alt[,"PATIENT_ID"])) / length(pt) * 100)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
