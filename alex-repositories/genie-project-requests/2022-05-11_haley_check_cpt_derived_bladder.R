# Description: Check cpt_oncotree_code for bladder in derived files.
# Author: Haley Hunter-Zinck
# Date: 2022-05-11

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)

# synapse
synid_file_cpt <- "syn22296823"
synid_table_cpt <- "syn21446709"
synid_file_upl <- "syn26250083"
synid_file_dd <- "syn26344720"
synid_file_exp <- "syn22314389"

# variables
cohort_str <- "BLADDER"

# functions ----------------------------

#' Download and load data stored in csv or other delimited format on Synapse
#' into an R data frame.
#' 
#' @param synapse_id Synapse ID
#' @version Version of the Synapse entity to download.  NA will load current
#' version
#' @param set Delimiter for file
#' @param na.strings Vector of strings to be read in as NA values
#' @param header TRUE if the file contains a header row; FALSE otherwise.
#' @param check_names TRUE if column names should be modified for compatibility 
#' with R upon reading; FALSE otherwise.
#' @param comment.char character designating comment lines to ignore
#' @return data frame
get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA,
                                           sep = ",", 
                                           na.strings = c("NA"), 
                                           header = T,
                                           check_names = F,
                                           comment.char = "#",
                                           colClasses = "character") {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = check_names,
                   header = header, comment.char = comment.char, colClasses = colClasses)
  return(data)
}

#' Store a file on Synapse with options to define provenance.
#' 
#' @param path Path to the file on the local machine.
#' @param parent_id Synapse ID of the folder or project to which to load the file.
#' @param file_name Name of the Synapse entity once loaded
#' @param prov_name Provenance short description title
#' @param prov_desc Provenance long description
#' @param prov_used Vector of Synapse IDs of data used to create the current
#' file to be loaded.
#' @param prov_exec String representing URL to script used to create the file.
#' @return Synapse ID of entity representing file
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
  
  return(file$properties$id)
}

# synapse login --------------------

status <- synLogin()

# read ----------------------------

df_cpt_der <- get_synapse_entity_data_in_csv(synid_file_cpt)

df_msk_bladder <- get_synapse_entity_data_in_csv(synid_file_upl)

df_dd <- get_synapse_entity_data_in_csv(synid_file_dd)

df_exp <- get_synapse_entity_data_in_csv(synid_file_exp)

# main ----------------------------

# derived variable view
res_der <- df_cpt_der %>% 
  filter(cohort == cohort_str) %>%
  select(cpt_oncotree_code) %>%
  group_by(cpt_oncotree_code) %>%
  count()
print(res_der)

# table view 
query <- glue("SELECT cpt_oncotree_code, COUNT(*) AS n FROM {synid_table_cpt} WHERE cohort = '{cohort_str}' GROUP BY cpt_oncotree_code ORDER BY cpt_oncotree_code")
res_tbl <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))
print(res_tbl)

# get patients with null codes
query <- glue("SELECT cpt_genie_sample_id FROM {synid_table_cpt} WHERE cohort = '{cohort_str}' AND cpt_oncotree_code IS NULL")
res_id <- as.character(unlist(as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))))

# upload view
res_upl <- df_msk_bladder %>% 
  filter(is.element(cpt_genie_sample_id, res_id)) %>%
  select(cpt_genie_sample_id, cpt_oncotree_code)
print(res_upl)

# is cpt_oncotree_code required? no
res_dd <- df_dd %>%
  filter(`Variable / Field Name` == "cpt_oncotree_code") %>%
  mutate(required = case_when(`Variable / Field Name` == "y" ~ "yes",
                              TRUE ~ "no"))
print(glue("cpt_oncotree_code required? {res_dd$required}"))

# check msk bladder genie export for oncotree codes
res_exp <- df_exp %>%
  filter(is.element(cpt_genie_sample_id, res_id)) %>%
  select(cpt_genie_sample_id, cpt_oncotree_code)
print(res_exp)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
