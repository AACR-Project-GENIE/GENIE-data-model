# Description: get drug names that are still unmapped.
# Author: Haley Hunter-Zinck
# Date: August 31, 2021
# request: 
#    source: I found some drugs in the bpc drug masking reports with fda status still "unknown"
#    reason: didn't look at drug names listed in the 'drugs_drug_oth_#' column


# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_table_drugs <- "syn21446703"
synid_table_mapping <- "syn26125434"
synid_table_concepts <- "syn26119153"
synid_folder_output <- "syn22285953"

# columns
columns_drug <- c(glue("drugs_drug_{c(1:5)}"), glue("drugs_drug_oth_{c(1:5)}"))

# functions ----------------------------

#' Get sorted unique values out of a synpase table for the specified columns.
#' 
#' @param synapse_id Synapse ID of the table
#' @param column_names Names of columns in the table.
#' @return Vector of unique values
#' @example 
#' get_unique_values(synapse_id = "syn12345", column_names = c("first_col", "second_col"))
get_unique_values <- function(synapse_id, column_names) {
  query <- glue("SELECT {paste0(column_names, collapse = ',')} FROM {synapse_id}")
  raw_values <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))
  uniq_values <- sort(unique(unlist(raw_values)))
  
  return(uniq_values)
}

regularize_drug_names <- function(drug_names_raw) {
  # lower case, remove white space, replace punctuation with white space
  mod <- tolower(drug_names_raw)
  mod <- gsub(pattern = "[[:space:]]", replacement = "", x = mod)
  mod <- gsub(pattern = "[[:punct:]]", replacement = " ", x = mod)
  return(mod)
}

get_unmapped_bpc <- function(synapse_id, drugs) {
  
  query <- glue("SELECT DISTINCT BPC FROM {synapse_id}")
  mapped <- as.character(unlist(as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))))
  reg_mapped <- regularize_drug_names(mapped)
  reg_drugs <- regularize_drug_names(drugs)
  
  reg_unmapped <- setdiff(reg_drugs, reg_mapped)
  unmapped <- as.character(setNames(drugs, reg_drugs)[reg_unmapped])
  
  return(unmapped)
}

save_to_synapse <- function(file_name, parent_id, prov_name = "", prov_desc = "", prov_used = "", prov_exec = "") {
  file <- File(path = file_name, parentId = parent_id)
  act <- Activity(name = prov_name,
                  description = prov_desc,
                  used = prov_used,
                  executed = prov_exec)
  file <- synStore(file, activity = act)
  
  return(T)
}

# unmapped BPC drug names ----------------------------

# get unmapped drug names in the BPC tables
bpc_drugs <- get_unique_values(synapse_id = synid_table_drugs, column_names = columns_drug)
bpc_unmapped <- get_unmapped_bpc(synapse_id = synid_table_mapping, drugs = bpc_drugs)

# write locally
local_csv <- "bpc_hemonc_unmapped.csv"
to_write <- cbind(bpc_unmapped, "")
colnames(to_write) <- c("bpc_unmapped", "hemonc")
write.csv(to_write, file = local_csv, row.names = F)

# load to synapse
save_to_synapse(file_name = local_csv, 
                parent_id = synid_folder_output, 
                prov_name = "unmapped BPC drugs", 
                prov_desc = "Get a list of BPC drugs that are unmapped in the HemOnc ontology", 
                prov_used = c(synid_table_drugs, synid_table_mapping), 
                prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2021-09-10_haley_unmapped_bpc_drugs.R")

# clean up
file.remove(local_csv)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
