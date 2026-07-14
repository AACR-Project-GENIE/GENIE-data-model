# Description: Check that the synapse IDs of specified entities in a config file
#     match intended entities by check parameters and names of entities.
# Author: Haley Hunter-Zinck
# Date: 2022-05-20

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(yaml)
library(dplyr)
library(synapser)

# synapse

# parameters
url <- "https://raw.githubusercontent.com/Sage-Bionetworks/genie-bpc-quac/gh-18-add-phase2-synids/config.yaml"
filename <- "config.yaml"

# functions ----------------------------

#' Get the name of a Synapse entity. 
#' 
#' @param synapse_id Synapse ID string
#' @return String representing entity name
#' @example get_synapse_entity_name("syn12345")
get_synapse_entity_name <- function(synapse_id) {
  return(synGet(synapse_id, downloadFile = F)$properties$name)
}


# synapse login --------------------

status <- synLogin()

# read ----------------------------

status <- download.file(url, destfile = filename, method = "wget")
config <- read_yaml(filename)

# main ----------------------------

for (cohort in names(config$uploads)) {
  for (site in names(config$uploads[[cohort]])) {
    entity_name <- get_synapse_entity_name(config$uploads[[cohort]][[site]]$data1)
    expected_name <- glue("{site} {cohort} Data")
    if (entity_name != expected_name) {
      print(glue("entity_name: {entity_name}"))
      print(glue("expected_name: {expected_name}"))
    }
  }
}

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
