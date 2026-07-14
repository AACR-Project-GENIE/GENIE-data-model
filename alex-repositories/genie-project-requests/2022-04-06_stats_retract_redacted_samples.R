# Description: Get IDs of all redacted samples from NSCLC and
#   add to reference table for temporary retraction. 
# Author: Haley Hunter-Zinck
# Date: 2022-04-06

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)

# synapse
synid_table_tmp <- "syn29266682"
synid_table_pat <- "syn21446700"

# parameters
cohort = "NSCLC"
comment = "NSCLC 2.0-public redacted patients"

# functions ----------------------------

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

synLogin()

# read ----------------------------

query <- glue("SELECT record_id, cohort FROM {synid_table_pat} WHERE cohort = '{cohort}' AND redacted = 'Yes'")
df_red <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))

# main ----------------------------

n_version <- create_synapse_table_version(table_id = synid_table_tmp, 
                             data = df_red, 
                             comment = comment, 
                             append = F)


# close out ----------------------------

print(glue("New table snapshot saved to {synid_table_tmp}.{n_version}"))

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
