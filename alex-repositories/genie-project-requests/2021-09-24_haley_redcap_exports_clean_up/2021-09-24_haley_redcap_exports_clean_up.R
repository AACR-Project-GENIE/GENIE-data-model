# Description: Create folder structure by cohort and site within the "External REDCap exports" folder
#    to facilitate file organization.
# Author: Haley Hunter-Zinck
# Date: 2021-09-23
# Request: 
#    Source: self-initialized
#    Date requested: 2021-09-23
#    Message: automate folder structure creation

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(yaml)
library(synapser)
synLogin()

# for upload synapse IDs
config <- read_yaml("config.yaml")

# parameters
synid_folder_exports <- config$synapse$exports$id
debug = config$misc$debug

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


# folder structure ----------------------------

cohorts <- sort(names(config$upload))
n_max_site <- max(unlist(lapply(config$upload, function(x) {length(names(x))})))
u_sites <- sort(unique(unlist(lapply(config$upload, function(x) {names(x)}))))

# store synapse ids for debugging
synid_folders <- matrix(NA, nrow = length(cohorts), ncol = n_max_site + 1, dimnames = list(cohorts, c("root", u_sites)))

# create folder structure
for (cohort in cohorts) {
  
  synid_folders[cohort, "root"] <- create_synapse_folder(name = cohort, parentId = synid_folder_exports)
  
  sites <- names(config$upload[[cohort]])
  for (site in sites) {
    synid_folders[cohort,site] <- create_synapse_folder(name = site, parentId = synid_folders[cohort, "root"])
  }
}

# organize current uploads ----------------------------

cohorts <- sort(names(config$upload))
for (cohort in cohorts) {
  
  sites <- names(config$upload[[cohort]])
  for (site in sites) {
    
    synid_dest <- get_synid_folder_cohort_site(synid_folder_root = config$synapse$exports$id, 
                                               cohort = cohort, 
                                               site = site)
    
    for (synid_file_upload in config$upload[[cohort]][[site]]) {
      
      synid_file_upload <- as.character(synid_file_upload)
      synid_file_children <- get_synapse_folder_children(synid_dest, include_types=list("file"))
      
      # check if file is already in destination folder
      if (!is.element(synid_file_upload, synid_file_children)) {
        
        res <- synMove(entity = as.character(synid_file_upload), new_parent = synid_dest)
        
        if (debug) {
          print(glue("File '{get_synapse_entity_name(synid_file_upload)}' ({synid_file_upload}) moved to {synid_dest}."))
        }
      }
    }
  }
}

# archive ----------------------------

# put remaining files in the archive folder
synid_dest <- config$synapse$archive$id
synid_source_children <- get_synapse_folder_children(synapse_id = synid_folder_exports, 
                                                            include_types = list("file"))
for (synid_source_child in synid_source_children) {
  
  synid_source_child <- as.character(synid_source_child)
  synid_dest_children <- get_synapse_folder_children(synapse_id = synid_dest, 
                                                     include_types = list("file"))
  
  if (!is.element(synid_source_child, synid_dest_children)) {
    
    res <- synMove(entity = synid_source_child, new_parent = synid_dest)
    
    if (debug) {
      cat(glue("File '{get_synapse_entity_name(synid_source_child)}' ({synid_source_child}) moved to folder '{get_synapse_entity_name(synid_dest)}' ({synid_dest})."), "\n")
    }
  }
}

# placeholder files for bladder ----------------------

cohort = "BLADDER"
u_sites <- sort(unique(unlist(lapply(config$upload, function(x) {names(x)}))))

# write placeholder file
file_placeholder <- "placeholder.csv"
write(c("This file contains no data.","It is a placeholder for future data files."), 
      file = file_placeholder, 
      ncolumns = 1)

for (site in u_sites) {
  
  synid_dest <- get_synid_folder_cohort_site(synid_folder_root = synid_folder_exports, 
                                             cohort = cohort, 
                                             site = site)
  save_to_synapse(path = file_placeholder, 
                              parent_id = synid_dest, 
                              file_name = glue("{site} {cohort} Data"))
  
  if (site == "DFCI") {
    save_to_synapse(path = file_placeholder, 
                    parent_id = synid_dest, 
                    file_name = glue("{site} {cohort} Header"))
  }
}

# clean up 
file.remove(file_placeholder)

# rename pre-existing upload file entities on Synpase ----------------------

# couldn't figure out how to do this programmatically so just change manually

# close out ----------------------------

print("Folders created: ")
print(synid_folders)

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
