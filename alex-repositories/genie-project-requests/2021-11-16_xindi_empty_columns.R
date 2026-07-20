# Description: determine which columns are completely empty in the tables.
# Author: Haley Hunter-Zinck
# Date: 2021-11-16

# setup -----------------

library(glue)
library(synapser)
synLogin()

# parameters
cohorts <- c("BrCa", "CRC", "NSCLC", "PANC", "Prostate")

# files
file_name_template <- "{tolower(cohort)}_all_table_all.csv"
file_output <- "2021-11-16_bpc_table_empty_column.csv"

# synapse
synid_folder_output <- "syn26162727"
synid_table_view <- "syn21446696"

# functions -------------------

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
#' @return TRUE if successful, otherwise return error.
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
  
  return(T)
}

get_bpc_table_synapse_ids <- function(synid_table_view) {
  query <- glue("SELECT id, name FROM {synid_table_view} WHERE double_curated = 'false'")
  table_info <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))
  
  return(setNames(table_info$id, table_info$name))
}

# main ----------------

empty <- list()
inter <- c()
for (cohort in cohorts) {
  file_name <- glue(file_name_template)
  data <- read.csv(file_name, stringsAsFactors = F)
  
  empty[[cohort]] <- data$value
  
  if (cohort != cohorts[1]) {
    inter <- intersect(inter, data$value)
  } else {
    inter <- data$value
  }
}

# main direct ------------------

synid_tables <- as.character(get_bpc_table_synapse_ids(synid_table_view))

empty_col <- list()
nonempty_col <- list()
for (synid_table in synid_tables) {
  query <- glue("SELECT * FROM {synid_table}")
  data <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))
  
  idx_empty <- which(apply(data, 2, function(x) {return(length(which(!is.na(x))) == 0)}))
  idx_nonempty <- which(apply(data, 2, function(x) {return(length(which(!is.na(x))) > 0)}))
  
  empty_col[[synid_table]] <- colnames(data)[idx_empty]
  nonempty_col[[synid_table]] <- colnames(data)[idx_nonempty]
}

col_root_empty <- unlist(lapply(strsplit(unlist(empty_col), split = "___"), head, n = 1))
col_root_not_empty <- unlist(lapply(strsplit(unlist(nonempty_col), split = "___"), head, n = 1))
col_empty <- setdiff(col_root_empty, col_root_not_empty)



# write --------------------

write(col_empty, ncolumns = 1, file = file_output)

save_to_synapse(path = file_output, 
                  parent_id = synid_folder_output,
                  prov_name = "empty columns", 
                  prov_desc = "columns that are entirely empty in the BPC Synapse tables", 
                  prov_used = as.character(get_bpc_table_synapse_ids(synid_table_view)), 
                  prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2021-11-16_xindi_empty_columns.R")

file.remove(file_output)
