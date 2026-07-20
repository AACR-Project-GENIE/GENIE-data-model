# Description: request to BPC domain expert to complete BPC to HemOnc mapping
#   for unmapped drugs.
# Author: Haley Hunter-Zinck
# Date: August 31, 2021
# email: 
#   Email from Michele Laura Lenoue-newton (michele.l.lenoue-newton@vumc.org)
#   Date sent: August 30, 2021 
#   Note: this script is for my response and request for a domain expert to
#     complete the BPC to HemOnc mapping.  


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
columns_drug <- glue("drugs_drug_{c(1:5)}")

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

get_hemonc_mapping_classes <- function(synid_table_mapping, synid_table_concepts) {
  
  query <- glue("SELECT DISTINCT LOWER(HemOnc_name) AS concept_name FROM {synid_table_mapping}")
  mapped_hemonc <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))
  
  query <- glue("SELECT LOWER(concept_name) AS concept_name, concept_class 
            FROM {synid_table_concepts} 
            WHERE vocabulary_name = 'HemOnc' 
                AND concept_name <> 'Investigational Drug'")
  concept_hemonc <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))
  
  res <- concept_hemonc %>% 
    inner_join(mapped_hemonc, by = "concept_name") %>% 
    select("concept_class") %>% 
    distinct()
  
  return(res)
}

get_hemonc_concepts <- function(synapse_id, classes) {
  
  class_set <- paste0("'", paste0(classes, collapse = "','"), "'", sep = "")
  
  query <- glue("SELECT concept_name 
                FROM {synapse_id} 
                WHERE vocabulary_name = 'HemOnc' 
                  and concept_class IN ({class_set}) ORDER BY concept_name")
  
  return(as.character(unlist(as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F)))))
}


# unmapped BPC drug names ----------------------------

# get unmapped drug names in the BPC tables
bpc_drugs <- get_unique_values(synapse_id = synid_table_drugs, column_names = columns_drug)
bpc_unmapped <- get_unmapped_bpc(synapse_id = synid_table_mapping, drugs = bpc_drugs)

hemonc_in_bpc <- get_hemonc_mappings()

# provenance
prov_name <- "unmapped BPC drugs"
prov_desc <- "Get a list of BPC drugs that are unmapped in the HemOnc ontology"
prov_used <- c(synid_table_drugs, synid_table_mapping)
prov_exec <- "https://github.com/hhunterzinck/genie_requests/blob/main/2021-08-30_michele_bpc_map.R"

# store file to synapse
local_csv <- "bpc_hemonc_unmapped.csv"
to_write <- cbind(bpc_unmapped, "")
colnames(to_write) <- c("bpc_unmapped", "hemonc")
write.csv(to_write, file = local_csv, row.names = F)

file <- File(path = local_csv, parentId = synid_folder_output)
act <- Activity(name = prov_name,
                description = prov_desc,
                used = prov_used,
                executed = prov_exec)
file <- synStore(file, activity = act)

# clean up
file.remove(local_csv)

# full relevant HemOnc concept list ----------------------------

# get mapped hemonc concept classes
hemonc_classes <- get_hemonc_mapping_classes(synid_table_mapping, synid_table_concepts)
hemonc_concepts <- get_hemonc_concepts(synapse_id = synid_table_concepts, classes = hemonc_classes)

# provenance
prov_name <- "Relevant HemOnc concepts"
prov_desc <- "Relevant HemOnc concepts for the BPC to HemOnc drug name mappings"
prov_used <- c(synid_table_mapping, synid_table_concepts, synid_table_concepts)
prov_exec <- "https://github.com/hhunterzinck/genie_requests/blob/main/2021-08-30_michele_bpc_map.R"

# store file to synapse
local_csv <- "hemonc_relevant_concepts.csv"
to_write <- data.frame(hemonc = hemonc_concepts)
write.csv(to_write, file = local_csv, row.names = F)

file <- File(path = local_csv, parentId = synid_folder_output)
act <- Activity(name = prov_name,
                description = prov_desc,
                used = prov_used,
                executed = prov_exec)
file <- synStore(file, activity = act)

# clean up
file.remove(local_csv)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
