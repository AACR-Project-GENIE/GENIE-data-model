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
synid_folders["Prostate"]<- "syn26256984"
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

get_ct_unmasked_drug_unique_count <- function(data) {
  reg_ct <- as.character(unlist(data %>%
                                  filter(drugs_ct_yn == "Yes") %>%
                                  select(drugs_in_regimen) %>% 
                                  distinct()))
  
  drugs_ct <- unique(unlist(strsplit(reg_ct, split = ", ")))
  drugs_ct <- drugs_ct[-which(drugs_ct == "Investigational Drug")]
  
  return(length(drugs_ct))
}

get_ct_unmasked_drug_instance_count <- function(data) {
  
  reg_all <- as.character(unlist(data %>%
                                   filter(drugs_ct_yn == "Yes") %>%
                                   select(drugs_in_regimen)))
  
  drugs_all <- unlist(strsplit(reg_all, split = ", "))
  drugs_all <- drugs_all[-which(drugs_all == "Investigational Drug")]
  
  return(length(drugs_all))
}

get_all_unmasked_drug_instance_count <- function(data) {
  
  reg_all <- as.character(unlist(data %>%
                                   select(drugs_in_regimen)))
  
  drugs_all <- unlist(strsplit(reg_all, split = ", "))
  drugs_all <- drugs_all[-which(drugs_all == "Investigational Drug")]
  
  return(length(drugs_all))
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
res_unique_count <- matrix(NA, nrow = length(cohorts), ncol = length(sites),
              dimnames = list(cohorts, sites))
res_instance_perc <- matrix(NA, nrow = length(cohorts), ncol = length(sites),
                           dimnames = list(cohorts, sites))

for (i in 1:nrow(res_unique_count)) {
  for (j in 1:ncol(res_unique_count)) {
    
    cohort <- cohorts[i]
    site <- sites[j]
    data <- datas[[cohort]][[site]]
    
    if (!is.null(data)) {
      res_unique_count[i,j] <- get_ct_unmasked_drug_unique_count(data)
      res_instance_perc[i,j] <- round(get_ct_unmasked_drug_instance_count(data) / get_all_unmasked_drug_instance_count(data) * 100.0, 2)
    }
  }
}

# write --------------------------------

# write unique counts
file_output <- "summary_unmasked_drug_unique_count_in_ct_regimen.csv"
write.csv(res_unique_count, file = file_output)
save_to_synapse(path = file_output, 
                parent_id = synid_folder_output, 
                prov_name = "count unmasked drugs", 
                prov_desc = "Number of unique unmasked drug names in regimens marked as in a clinical trial", 
                prov_used = as.character(synid_folders), 
                prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2021-10-20_mike_drug_masking_blanket_all_cohorts.R")
file.remove(file_output)

# write instance percentages
file_output <- "summary_unmasked_drug_instance_perc_in_ct_regimen.csv"
write.csv(res_instance_perc, file = file_output)
save_to_synapse(path = file_output, 
                parent_id = synid_folder_output, 
                prov_name = "percentage unmasked drugs", 
                prov_desc = "Percentage of instances of unmasked drug names in regimens marked as in a clinical trial out of all instances of drugs mentioned", 
                prov_used = as.character(synid_folders), 
                prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2021-10-20_mike_drug_masking_blanket_all_cohorts.R")
file.remove(file_output)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
