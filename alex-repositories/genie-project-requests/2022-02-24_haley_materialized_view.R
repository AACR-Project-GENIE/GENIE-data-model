# Description: Experiment with new Materialized Views to conduct SQL joings
#   in Synapse. 
# Confluence: https://sagebionetworks.jira.com/wiki/spaces/PLFM/pages/2514321554/Synapse+Joins
# Note: as of date of this script, only in Synapse staging so did not create view in production
# Author: Haley Hunter-Zinck
# Date: 2022-02-24

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_project_test <- "syn25921894"
synid_table_pt <- "syn27240986"
synid_table_file <- "syn27240957"

# parameters
name <- "my-r-materialized-view"

# functions ----------------------------

store_synapse_join <- function(query, parent_id, name) {
  json_str <- glue('{
    "concreteType":"org.sagebionetworks.repo.model.table.MaterializedView",
    "name":"{{name}}",
    "parentId":"{{parent_id}}",
    "definingSQL":"{{query}}"
  }', .open = "{{", .close = "}}")
  synid_mv_join <- synRestPOST(uri="/entity", 
                        body = json_str)
  return(synid_mv_join)
}

# main ----------------------------

query <- glue("SELECT F.patient_id AS patient_id, F.patient_age AS patient_age, P.file_id AS file_id, P.name FROM {synid_table_pt} AS F JOIN {synid_table_file} AS P ON (P.patient_id = F.patient_id)")
synid_mv_join <- store_synapse_join(query = query, parent_id = synid_project_test, name = name)

# close out ----------------------------

print(glue("Synapse ID of join: {synid_mv_join}"))

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
