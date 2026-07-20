# Description: Remove rows with empty start dates from cbio treatment 
#   timelines files and re-upload. 
# Author: Haley Hunter-Zinck
# Date: 2022-02-03

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_files <- setNames(c("syn26349077", "syn26475421", "syn26990161"), c("panc","prostate","bladder"))

# parameters
file_name <- "data_timeline_treatment.txt"

# for checking
n_rm <- setNames(c(1,4,3), c("panc","prostate","bladder"))

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

raw <- list()
for (i in 1:length(synid_files)) {
  cohort <- names(synid_files)[i]
  raw[[cohort]] <- get_synapse_entity_data_in_csv(as.character(synid_files[cohort]), sep = "\t")
}

# main ----------------------------

mod <- list()

for (i in 1:length(raw)) {
  mod[[names(raw)[i]]] <- raw[[i]] %>% 
    filter(!is.na(START_DATE)) 
}


# tests -----------------------------

res_check <- c()
for (i in 1:length(mod)) {
  if (nrow(raw[[i]]) - nrow(mod[[i]]) == n_rm[names(mod[i])]) {
    res_check[names(mod[i])] = T
  } else {
    res_check[names(mod[i])] = T
  }
}

# write -------------------------------

if (all(res_check)) {
  
  for (i in 1:length(mod)) {
    
    cohort <- names(mod)[i]
    write.table(mod[[i]], file = file_name, row.names = F, sep = "\t", na = "", quote = F)
    
    synid_folder_output <- synGet(as.character(synid_files[cohort]), downloadFile = F)$properties$parentId
    save_to_synapse(path = file_name, 
                    parent_id = synid_folder_output, 
                    prov_name = 'remove na start date', 
                    prov_desc = 'removes any rows in which start date is missing', 
                    prov_used = as.character(synid_files[cohort]), 
                    prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2022-02-03_haley_cbio_null_start_dates.R")
  }
}

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
