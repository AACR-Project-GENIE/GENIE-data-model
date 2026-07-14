# Description: For regimens marked as being a part of a clincial trial, what number
#   of unique drug names are unmasked?
# Author: Haley Hunter-Zinck
# Date: 2021-10-20

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_folder_output <- "syn26380175"
synid_folders <- c()
synid_folders["BrCa"]<- "syn26195593"
synid_folders["CRC"]<- "syn26132942"
synid_folders["Prostate"] <- "syn26256984"
synid_folders["NSCLC"] <- "syn26380225"

# functions ----------------------------

get_synapse_folder_children <- function(synapse_id, 
                                        include_types=list("folder", "file", "table", "link", "entityview", "dockerrepo")) {
  
  ent <- as.list(synGetChildren(synapse_id, includeTypes = include_types))
  
  children <- c()
  
  if (length(ent) > 0) {
    for (i in 1:length(ent)) {
      children[ent[[i]]$name] <- ent[[i]]$id
    }
  }
  
  return(children)
}

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

get_synapse_entity_name <- function(synapse_id) {
  return(synGet(synapse_id, downloadFile = F)$properties$name)
}

get_site <- function(x) {
  if (length(grep(pattern = "DFCI", x = x))) {
    return ("DFCI")
  }
  
  if (length(grep(pattern = "MSK", x = x))) {
    return ("MSK")
  }
  
  if (length(grep(pattern = "UHN", x = x))) {
    return ("UHN")
  }
  
  return("VICC")
}

get_number_ct_unmasked <- function(data) {
  reg_ct <- as.character(unlist(data %>%
                                  filter(drugs_ct_yn == "Yes") %>%
                                  select(drugs_in_regimen)))
  
  drugs_ct <- unlist(strsplit(reg_ct, split = ", "))
  
  return(table(drugs_ct))
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

# read ----------------------------

datas <- list()

for (i in 1:length(synid_folders)) {
  
  synid_folder_children <- get_synapse_folder_children(as.character(synid_folders[i]), 
                                                      include_types = list("file"))
  
  idx_synid_files_csv <- grep(pattern = "\\.csv$", x = names(synid_folder_children), 
                              value = F)
  
  datas[[names(synid_folders)[i]]] <- list()
  
  for (idx in idx_synid_files_csv) {
    
    site <- get_site(names(synid_folder_children)[idx])
    datas[[names(synid_folders)[i]]][[site]] <- get_synapse_entity_data_in_csv(as.character(synid_folder_children[idx]))
  }
}

# main ----------------------------

cohorts <- sort(names(datas))
sites <- sort(unique(unlist(lapply(datas, names))))
res <- list()

for (site in sites) {
  res[[site]] <- list()
  
  res_cohort <- list()
  for (cohort in cohorts) {
    data <- datas[[cohort]][[site]]
    
    if (!is.null(data)) {
      res_cohort[[cohort]] <- get_number_ct_unmasked(data)
    }
  }
  
  drugs <- sort(unique(unlist(lapply(res_cohort, names))))
  res[[site]] <- matrix(0, nrow = length(drugs), ncol = length(cohorts),
                        dimnames = list(drugs, cohorts))
  for (cohort in cohorts) {
    if (is.null(datas[[cohort]][[site]])) {
      res[[site]][,cohort] <- NA
    } else {
      res[[site]][match(names(res_cohort[[cohort]]), drugs), cohort] <- res_cohort[[cohort]]
    }
  }
}

# write --------------------------------

for (site in names(res)) {
  
  file_output <- glue("{tolower(site)}_unmasked_drug_count_in_ct_regimen.csv")
  write.csv(res[[site]], file = file_output)
  
  save_to_synapse(path = file_output,
                  parent_id = synid_folder_output,
                  prov_name = "count unmasked drugs",
                  prov_desc = "Number of unmasked drug name instances in regimens marked as in a clinical trial",
                  prov_used = as.character(synid_folders),
                  prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2021-10-20_shawn_drug_masking_blanket_all_cohorts.R")
  
  file.remove(file_output)
}

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
