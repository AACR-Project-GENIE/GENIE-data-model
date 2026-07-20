# Description: Provide a list of all therapies indicated for Breast Cancer
#      as noted in the HemOnc ontology.  
# Author: Haley Hunter-Zinck
# Date: September 10, 2021
# Request:
#    Source: slack from Shawn
#    Date requested: 2021-09-10
#    Message: How long would it take you to use the hemonc tables to generate 
#       a list of therapies used for breast cancer?

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(synapser)
synLogin()
library(dplyr)

# synapse
synid_table_rel <- "syn26119155"
synid_table_con <- "syn26119153"
synid_table_map <- "syn26125434"
synid_folder_output <- "syn26162727"

# values
REL_INDICATION <- 'Has FDA indication'

# files
file_output <- "hemonc_brca_therapies.csv"

# functions ----------------------------

save_to_synapse <- function(file_name, parent_id, prov_name = "", prov_desc = "", prov_used = "", prov_exec = "") {
  file <- File(path = file_name, parentId = parent_id)
  act <- Activity(name = prov_name,
                  description = prov_desc,
                  used = prov_used,
                  executed = prov_exec)
  file <- synStore(file, activity = act)
  
  return(T)
}

# read ----------------------------

query <- glue("SELECT * FROM {synid_table_rel}")
relationships <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))

query <- glue("SELECT * FROM {synid_table_con}")
concepts <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))

query <- glue("SELECT * FROM {synid_table_map}")
bpc_map <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))

# main ----------------------------

concept_brca <- concepts %>%
  filter(concept_class == 'Condition') %>%
  filter(grepl(pattern = "breast", ignore.case = T, x = concept_name)) %>%
  select(concept_name, concept_id)

brca_drugs <- relationships %>%
  mutate(concept_id_1_str = as.character(concept_id_1)) %>%
  filter(relationship_name == REL_INDICATION) %>%
  filter(is.element(concept_id_2, concept_brca$concept_id)) %>%
  inner_join(concepts, by = c("concept_id_1" = "concept_id")) %>%
  left_join(bpc_map, by = c("concept_id_1_str" = "HemOnc_code")) %>%
  rename(hemonc_name = concept_name) %>%
  rename(bpc_name = BPC) %>%
  select(hemonc_name, concept_id_2, bpc_name) %>%
  distinct() %>%
  arrange(hemonc_name)

output <- brca_drugs %>%
  mutate(concept_id_2_int = as.integer(concept_id_2)) %>%
  inner_join(concepts, by = c("concept_id_2_int" = "concept_id")) %>%
  mutate(cancer_type = concept_name) %>%
  select(hemonc_name, cancer_type, bpc_name)

# write -------------------------------

write.csv(output, row.names = F, file = file_output)

save_to_synapse(file_name = file_output, 
                parent_id = synid_folder_output, 
                prov_name = "breast cancer therapies", 
                prov_desc = "all therapies indicated for breast cancer in the HemOnc ontology", 
                prov_used = c(synid_table_rel, synid_table_con, synid_table_map), 
                prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2021-09-10_shawn_hemonc_brca_regimens.R") 
  
  
# close out ----------------------------

file.remove(file_output)

toc = as.double(Sys.time())
print(glue("File '{file_output}' saved to {synid_folder_output}"))
print(glue("Runtime: {round(toc - tic)} s"))
