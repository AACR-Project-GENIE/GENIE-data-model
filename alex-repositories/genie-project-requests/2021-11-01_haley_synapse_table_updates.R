# Description: Test if I can wipe and replace all data in a table.  
# Author: Haley Hunter-Zinck
# Date: 2021-11-01

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_table_test <- "syn25944660"

# functions ----------------------------

#' Create a Synapse table snapshot version with comment.
#' 
#' @param table_id Synapse ID of a table entity
#' @param comment Message to annotate the new table version
#' @return snapshot version number
#' @example 
#' create_synapse_table_snapshot("syn12345", comment = "my new snapshot")
create_synapse_table_snapshot <- function(table_id, comment) {
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
create_new_table_version <- function(table_id, data, comment = "") {
  n_rm <- clear_synapse_table(table_id)
  n_add <- update_synapse_table(table_id, data)
  n_version <- create_synapse_table_snapshot(table_id, comment)
  return(n_version)
}

# main ---------------------------------

# get new dataset
old_data <- as.data.frame(synTableQuery(glue("SELECT * FROM {synid_table_test}.1"),  
                                        includeRowIdAndRowVersion = F))
new_data <- old_data
new_data[,1] <- sample(old_data[,1], size = nrow(old_data))

print(new_data)
file_local <- "local.csv"
write.csv(new_data, file_local, row.names = F)
new_data <- read.csv(file_local)

# refresh table
n_version <- create_new_table_version(table_id = synid_table_test, 
                                     data = new_data, 
                                     comment = "automated refresh from csv")

# close out ----------------------------

print(glue("Synapse table {synid_table_test} has been updated to version {n_version}."))
toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
