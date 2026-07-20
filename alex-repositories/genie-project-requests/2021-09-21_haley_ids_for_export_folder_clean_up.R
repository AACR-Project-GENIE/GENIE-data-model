# Description: Create folder structure by structure and site within the "IDs for Export" folder
#    to facilitate file organization.
# Author: Haley Hunter-Zinck
# Date: 2021-09-21
# Request: 
#    Source: self-initialized
#    Date requested: 2021-09-21
#    Message: automate folder structure creation

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_folder_ids <- "syn20781633"

# parameters
cohort_sites <- list()
cohort_sites[["BLADDER"]] <- c("all", "DFCI", "MSK", "UHN", "VICC")
cohort_sites[["BrCa"]] <- c("all", "DFCI", "MSK", "VICC")
cohort_sites[["CRC"]] <- c("all", "DFCI", "MSK", "VICC")
cohort_sites[["NSCLC"]] <- c("all", "DFCI", "MSK", "UHN", "VICC")
cohort_sites[["PANC"]] <- c("all", "DFCI", "MSK", "UHN", "VICC")
cohort_sites[["Prostate"]] <- c("all", "DFCI", "MSK", "UHN", "VICC")

# debugging
debug = TRUE

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

create_synapse_folder <- function(name, parentId) {
  
  # check if folder already exists
  children <- get_synapse_folder_children(parentId, include_types = list("folder"))
  if(is.element(name, names(children))) {
    return(as.character(children[name]))
  }
  
  concreteType <- "org.sagebionetworks.repo.model.Folder"
  uri <- "/entity"
  payload <- paste0("{", glue("'name':'{name}', 'parentId':'{parentId}', 'concreteType':'{concreteType}'"), "}")
  ent <- synRestPOST(uri = uri, body = payload)
  return(ent$id)
}

get_synid_folder_cohort_site <- function(synid_folder_root, cohort, site) {
  
  synid_folder_children <- get_synapse_folder_children(synid_folder_root, include_types = list("folder"))
  synid_folder_cohort <- as.character(synid_folder_children[[cohort]])
  
  synid_folder_children <- get_synapse_folder_children(synid_folder_cohort, include_types = list("folder"))
  synid_folder_site <- as.character(synid_folder_children[[site]])
  
  return(synid_folder_site)
}

# main ----------------------------

# create folder structure
synid_folders <- matrix(NA, nrow = length(cohort_sites), ncol = max(unlist(lapply(cohort_sites, length))) + 1, 
                        dimnames = list(sort(names(cohort_sites)), c("root", sort(unique(unlist(cohort_sites))))))
for (cohort in names(cohort_sites)) {
  
  synid_folders[cohort, "root"] <- create_synapse_folder(name = cohort, parentId = synid_folder_ids)
  
  for (site in cohort_sites[[cohort]]) {
    synid_folders[cohort,site] <- create_synapse_folder(name = site, parentId = synid_folders[cohort, "root"])
  }
}

# sort files
synid_file_children <- get_synapse_folder_children(synapse_id = synid_folder_ids, 
                                                            include_types = list("file"))
for (synid_file_child in synid_file_children) {
  
  ann <- synGetAnnotations(synGet(as.character(synid_file_child)))
  ann_names <- names(ann)
  
  if (!length(setdiff(c("cohort", "site"), ann_names))) {
    
    cohort <- ann$cohort[[1]]
    site <- ann$site[[1]]
    
    if (cohort == "PANCREAS") {
      cohort = "PANC"
    } else if (cohort == "PROSTATE") {
      cohort = "Prostate"
    }
    
    synid_dest <- get_synid_folder_cohort_site(synid_folder_root = synid_folder_ids,
                                 cohort = cohort,
                                 site = site)
    res <- synMove(entity = as.character(synid_file_child), new_parent = synid_dest)
    
    if (debug) {
      cat(glue("File '{names(synid_file_child)}' ({as.character(synid_file_child)}) moved to {synid_dest}."), "\n")
    }
    
  } else {
    if (debug) {
      cat(glue("File '{names(synid_file_child)}' ({as.character(synid_file_child)}) not moved"), "\n")
    }
  }
}

# close out ----------------------------

print("Folders created: ")
print(synid_folders)

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
