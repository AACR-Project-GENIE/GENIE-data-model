# Description: Investigate currently invalid files.
# Author: Haley Hunter-Zinck
# Date: 2022-03-07

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(yaml)
library(synapser)
synLogin()

# synapse
synid_table_invalid <- "syn10153306"
synid_table_upload <- "syn26592118"

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
                                           comment.char = "#") {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = check_names,
                   header = header, comment.char = comment.char)
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

#' Remove leading and trailing whitespace from a string.
#' @param str String
#' @return String without leading or trailing whitespace
trim <- function(str) {
  front <- gsub(pattern = "^[[:space:]]+", replacement = "", x = str)
  back <- gsub(pattern = "[[:space:]]+$", replacement = "", x = front)
  
  return(back)
}

# read ----------------------------

query <- glue("SELECT center, id FROM {synid_table_invalid} WHERE fileType = 'assayinfo'")
df_synid_assay <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F)) %>%
  rename(id_assay = id)

query <- glue("SELECT id, name FROM {synid_table_upload} WHERE name LIKE 'data_clinical_supp%' AND name NOT LIKE '%patient%'")
df_synid_sam <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))

# check assay files ----------------------------

df_synid_sam_center <- df_synid_sam %>% 
  mutate(center = unlist(lapply(strsplit(df_synid_sam$name, split = "[_.]"), FUN = function(x) {return(x[length(x)-1])}))) %>%
  rename(id_sam = id)

df_synid_assay_sam <- df_synid_assay %>%
  inner_join(df_synid_sam_center, by = "center") %>%
  select(center, id_assay, id_sam)
  
res_assay <- list()
for (i in 1:nrow(df_synid_assay_sam)) {
  df_assay <- read_yaml(synGet(df_synid_assay_sam[i,"id_assay"])$path)
  assay_labels <- trim(unlist(lapply(lapply(df_assay, function(x) {x$assay_specific_info}), function(x) {return(x[[1]]$SEQ_ASSAY_ID)})))
  
  sam <- get_synapse_entity_data_in_csv(df_synid_assay_sam[i,"id_sam"], sep = "\t")
  colnames(sam) = toupper(colnames(sam))
  sam_labels <- trim(unique(sam$SEQ_ASSAY_ID))
  res_assay[[df_synid_assay_sam[i,"center"]]] <- setdiff(sam_labels, assay_labels)
}

print(res_assay)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
