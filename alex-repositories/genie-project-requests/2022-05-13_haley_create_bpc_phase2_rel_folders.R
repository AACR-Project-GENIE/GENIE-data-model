# Description: Create BPC phase 2 release folders.  
# Author: Haley Hunter-Zinck
# Date: 2022-05-13

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)

# synapse
synid_folder_bpc <- "syn21241322"
synid_table_rel <- "syn27628075"
synid_version_rel <- 5

# parameters
cohorts <- c("CRC2", "ESOPHAGO", "MELANOMA", "NSCLC2", "OVARIAN", "RENAL")

# functions ----------------------------

#' Get the name of a Synapse entity. 
#' 
#' @param synapse_id Synapse ID string
#' @return String representing entity name
#' @example get_synapse_entity_name("syn12345")
get_synapse_entity_name <- function(synapse_id) {
  return(synGet(synapse_id, downloadFile = F)$properties$name)
}

#' Get all child entities of a synapse folder.
#' 
#' @param synapse_id Synapse ID of the folder
#' @param include_types Types of child entities to return
#' @return Vector with values as Synapse IDs and names as entity names.
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

#' Create a new Synape Folder entity. 
#' 
#' @param name Name of the Synapse Folder entity
#' @param parentId Synapse ID of Project or Folder in which to create the new Folder
#' @return Synapse ID of the new Synapse Folder entity
create_synapse_folder <- function(name, parent_id) {
  
  # check if folder already exists
  children <- get_synapse_folder_children(parent_id, include_types = list("folder"))
  if(is.element(name, names(children))) {
    return(as.character(children[name]))
  }
  
  concreteType <- "org.sagebionetworks.repo.model.Folder"
  uri <- "/entity"
  payload <- paste0("{", glue("'name':'{name}', 'parentId':'{parent_id}', 'concreteType':'{concreteType}'"), "}")
  ent <- synRestPOST(uri = uri, body = payload)
  return(ent$id)
}

#' Create a Synapse table snapshot version with comment.
#' 
#' @param table_id Synapse ID of a table entity
#' @param comment Message to annotate the new table version
#' @return snapshot version number
#' @example 
#' create_synapse_table_snapshot("syn12345", comment = "my new snapshot")
snapshot_synapse_table <- function(table_id, comment) {
  res <- synRestPOST(glue("/entity/{table_id}/table/snapshot"), 
                     body = glue("{'snapshotComment':'{{comment}}'}", 
                                 .open = "{{", 
                                 .close = "}}"))
  
  return(res$snapshotVersionNumber)
}

#' Clear all rows from a Synapse table.
#' 
#' @param table_id Synapse ID of a table
#' @return Number of rows deleted
clear_synapse_table <- function(table_id) {
  
  res <- as.data.frame(synTableQuery(glue("SELECT * FROM {table_id}")))
  tbl <- Table(schema = synGet(table_id), values = res)
  synDelete(tbl)
  
  return(nrow(res))
}

#' Update rows of a Synapse table with new data.
#' 
#' @param table_id Synapse ID of a table
#' @param data Data frame of new data
#' @return Number of rows added
update_synapse_table <- function(table_id, data) {
  
  entity <- synGet(table_id)
  project_id <- entity$properties$parentId
  table_name <- entity$properties$name
  table_object <- synBuildTable(table_name, project_id, data)
  synStore(table_object)
  
  return(nrow(data))
}

#' Clear all data from a table, replace with new data, and 
#' create a new snapshot version.
#' 
#' @param table_id Synapse ID of the table
#' @param data Data frame of new data
#' @param comment Comment string to include with the new snapshot version.
#' @return New snapshot version number
create_synapse_table_version <- function(table_id, data, comment = "", append = T) {
  
  if (!append) {
    n_rm <- clear_synapse_table(table_id)
  }
  n_add <- update_synapse_table(table_id, data)
  n_version <- snapshot_synapse_table(table_id, comment)
  return(n_version)
}

# synapse login --------------------

status <- synLogin()

# read ----------------------------

synid_folder_children <- get_synapse_folder_children(synapse_id = synid_folder_bpc, include_types=list("folder"))

# main ----------------------------

labels <- unlist(lapply(as.list(synGetColumns(synid_table_rel)), function(x) {x$name}))
mat_info <- matrix(NA, nrow = 0, ncol = length(labels), dimnames = list(c(), labels))

for (cohort in cohorts) {
  
  # create release folder 
  synid_folder_cohort <- as.character(synid_folder_children[cohort])
  synid_folder_1 <- create_synapse_folder("1.1-consortium", synid_folder_cohort)
  
  # create release subfolders
  synid_folder_1_cbio <- create_synapse_folder("cBioPortal_files", synid_folder_1)
  synid_folder_1_doc <- create_synapse_folder("Documentation", synid_folder_1)
  synid_folder_1_cli <- create_synapse_folder(glue("{cohort}_1.1-consortium_clinical_data"), synid_folder_1)
  
  # add table information for later upload
  vec_info <- setNames(rep(NA, length(labels)), labels)
  vec_info["cohort"] <- cohort
  vec_info["release_version"] <- "1.1"
  vec_info["release_type"] <- "consortium"
  vec_info["current"] <- "true"
  vec_info["clinical_file_folder"] <- synid_folder_1_cli
  mat_info <- rbind(mat_info, vec_info)
}

# update release table information
df_v5 <- as.data.frame(synTableQuery(glue("SELECT * FROM {synid_table_rel}.{synid_version_rel}"),
                                     includeRowIdAndRowVersion = F))
df_info <- data.frame(mat_info, stringsAsFactors = F)
df_info[,"current"] <- as.logical(df_info[,"current"])
df_write <- df_v5 %>%
  bind_rows(df_info)
n_version <- create_synapse_table_version(table_id = synid_table_rel, data = df_write, comment = "phase 2 placeholder folders", append = F)

# close out ----------------------------

print(glue("New version of table saved to {synid_table_rel}.{n_version}"))

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
