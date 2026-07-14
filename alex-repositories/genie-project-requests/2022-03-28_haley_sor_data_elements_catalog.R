# Description: Check for inconsistencies betweeen data elements catalog and the scope of release.
# Author: Haley Hunter-Zinck
# Date: 2022-03-28


# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_sor <- "syn22294851"
synid_table_ele <- "syn21431364"
synid_table_rel <- "syn27628075"
synid_table_cbio <- "syn25712693"

# parameters 
val_yes_sor = c("yes","always","index cancer only","non-index cancer only")

# functions ----------------------------

#' Read contents of an Excel Spreadsheet stored on Synapse.
#' 
#' @param synapse_id Synapse ID of the spreadsheet
#' @param version Version of the file
#' @param sheet Number of the sheet of the spreadsheet
#' @param check.names Whether R should modify names if non-conforming
#' @return Matrix of data
#' @example 
#' get_synapse_entity_data_in_xlsx(synapse_id = "syn123345", sheet = 2)
get_synapse_entity_data_in_xlsx <- function(synapse_id, 
                                            version = NA,
                                            sheet = 1,
                                            check.names = F) {
  library(openxlsx)
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.xlsx(entity$path, check.names = check.names, sheet = sheet)
  
  return(data)
}

#' Remove leading and trailing whitespace from a string.
#' @param str String
#' @return String without leading or trailing whitespace
trim <- function(str) {
  front <- gsub(pattern = "^[[:space:]]+", replacement = "", x = str)
  back <- gsub(pattern = "[[:space:]]+$", replacement = "", x = front)
  
  return(back)
}

# read ----------------------------

sor <- get_synapse_entity_data_in_xlsx(synid_file_sor, sheet = 2)
ele <- as.data.frame(synTableQuery(glue("SELECT * FROM {synid_table_ele}"), includeRowIdAndRowVersion = F))
release_info <- as.data.frame(synTableQuery(glue("SELECT cohort, release_version, release_type FROM {synid_table_rel} WHERE current = 'true'"), includeRowIdAndRowVersion = F))

# missing variables ----------------------------

# get variables
var_sor <- sor %>% 
  mutate(mut_var = trim(gsub(pattern = "___[0-9]+$", replacement = '', x = VARNAME))) %>%
  filter(!grepl(pattern = "_complete$", x = mut_var) & !grepl(pattern = "^synapse_tables_", x = mut_var)) %>%
  select(mut_var) %>%
  distinct()
var_ele <- unique(trim(as.character(unlist(ele %>% select(variable)))))

# look at variable name differences
ele_not_sor <- setdiff(unlist(var_ele), unlist(var_sor))
sor_not_ele <- setdiff(unlist(var_sor), unlist(var_ele))

# release status for NSCLC 2.0-public ----------------

col_cohort_sor <- "Shared.for.cBio.NSCLC.2.0-Public.Release"
val_yes_ele <- c("public")

# get release status
rel_sor <- sor %>% 
  mutate(mut_var = trim(gsub(pattern = "___[0-9]+$", replacement = '', x = VARNAME))) %>%
  mutate(mut_rel = is.element(trim(tolower(!!as.symbol(col_cohort_sor))), val_yes_sor)) %>%
  filter(!grepl(pattern = "_complete$", x = mut_var) & !grepl(pattern = "^synapse_tables_", x = mut_var)) %>%
  select(mut_var, mut_rel) 
rel_ele <- ele %>%
  mutate(mut_rel = is.element(NSCLC_sor, val_yes_ele)) %>%
  select(variable, mut_rel)

idx_dup <- which(duplicated(rel_sor$mut_var))
rel_sor_nodup <- rel_sor[-idx_dup,]

# get released variables
rel_sor_true <- rel_sor_nodup %>% 
  filter(mut_rel) %>%
  select(mut_var) %>%
  rename(variable = mut_var)
rel_ele_true <- rel_ele %>%
  filter(mut_rel) %>%
  select(variable)

# compare release status
inter_sor_ele <- intersect(unlist(var_sor), var_ele)
rel_sor_not_ele <- intersect(inter_sor_ele, setdiff(unlist(rel_sor_true), unlist(rel_ele_true)))
rel_ele_not_sor <- intersect(inter_sor_ele, setdiff(unlist(rel_ele_true), unlist(rel_sor_true)))

# release status for PANC 1.1-consortium ----------------

col_cohort_sor <- "Cbio_PANC_V1"
val_yes_ele <- c("public", "consortium", "project")

# get release status
rel_sor <- sor %>% 
  mutate(mut_var = trim(gsub(pattern = "___[0-9]+$", replacement = '', x = VARNAME))) %>%
  mutate(mut_rel = is.element(trim(tolower(!!as.symbol(col_cohort_sor))), val_yes_sor)) %>%
  filter(!grepl(pattern = "_complete$", x = mut_var) & !grepl(pattern = "^synapse_tables_", x = mut_var)) %>%
  select(mut_var, mut_rel) 
rel_ele <- ele %>%
  mutate(mut_rel = is.element(PANC_sor, val_yes_ele)) %>%
  select(variable, mut_rel)

idx_dup <- which(duplicated(rel_sor$mut_var))
rel_sor_nodup <- rel_sor[-idx_dup,]

# get released variables
rel_sor_true <- rel_sor_nodup %>% 
  filter(mut_rel) %>%
  select(mut_var) %>%
  rename(variable = mut_var)
rel_ele_true <- rel_ele %>%
  filter(mut_rel) %>%
  select(variable)

# compare release status
inter_sor_ele <- intersect(unlist(var_sor), var_ele)
rel_sor_not_ele <- intersect(inter_sor_ele, setdiff(unlist(rel_sor_true), unlist(rel_ele_true)))
rel_ele_not_sor <- intersect(inter_sor_ele, setdiff(unlist(rel_ele_true), unlist(rel_sor_true)))


# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
