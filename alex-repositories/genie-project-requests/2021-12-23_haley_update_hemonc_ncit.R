# Description: Update the NCIT to HemOnc mapping for missing NCIT codes.
# Author: Haley Hunter-Zinck
# Date: 2021-12-23

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_table_map <- "syn26125434"
synid_file_dd <- "syn26469280"

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
#' @return data frame
get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA,
                                           sep = ",", 
                                           na.strings = c("NA"), 
                                           header = T,
                                           check_names = F) {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = check_names,
                   header = header)
  return(data)
}

#' Create a Synapse table snapshot version with comment.
#' 
#' @param table_id Synapse ID of a table entity
#' @param comment Message to annotate the new table version
#' @return snapshot version number
#' @example 
#' create_synapse_table_snapshot("syn12345", comment = "my new snapshot")
snapshot_synapse_table <- function(table_id, comment) {
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
create_synapse_table_version <- function(table_id, data, comment = "", append = T) {
  
  if (!append) {
    n_rm <- clear_synapse_table(table_id)
  }
  n_add <- update_synapse_table(table_id, data)
  n_version <- snapshot_synapse_table(table_id, comment)
  return(n_version)
}

trim_string <- function(str) {
  front <- gsub(pattern = "^[[:space:]]+", replacement = "", x = str)
  back <- gsub(pattern = "[[:space:]]+$", replacement = "", x = front)
  
  return(back)
}

merge_last_elements <- function(x, delim) {
  
  y <- c()
  y[1] = x[1]
  y[2] <- paste0(x[2:length(x)], collapse = delim)
  return(y)
}

#' Perform string split operation but only on the first
#' occurrence of the split character.
strsplit_first <- function(x, split) {
  
  unmerged <- strsplit(x = x, split = split)
  remerge <- lapply(unmerged, merge_last_elements, delim = split)
  
  return(remerge)
}

parse_mapping <- function(str) {
  
  clean <- trim_string(gsub(pattern = "\"", replacement = "", x = str))
  splt <- strsplit_first(strsplit(x = clean, split = "|", fixed = T)[[1]], split = ",")
  
  codes <- unlist(lapply(splt, FUN = function(x) {return(trim_string(x[1]))}))
  values <- unlist(lapply(splt, FUN = function(x) {return(trim_string(x[2]))}))
  mapping <- data.frame(cbind(codes, values), stringsAsFactors = F)
  
  return(mapping)
}

# read ----------------------------

dd <- get_synapse_entity_data_in_csv(synid_file_dd)

query <- glue("SELECT * FROM {synid_table_map}")
ncit_hemonc <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))

# main ----------------------------

choices <- unlist(dd %>% filter(`Variable / Field Name` == 'drugs_drug_1') %>% select(`Choices, Calculations, OR Slider Labels`))
mapping <- parse_mapping(choices)

new_map <- ncit_hemonc %>% 
  left_join(mapping, by = c("BPC" = "values")) %>%
  mutate(c_codes = case_when(!is.na(codes) ~ paste0("C", codes),
                            TRUE ~ NCIT)) %>%
  select(BPC, c_codes, HemOnc_code) %>%
  rename(NCIT = c_codes)


#duplicated(new_map$NCIT)
#new_map %>% filter(!is.na(NCIT) & duplicated(NCIT)) %>% select(BPC, HemOnc_code, NCIT)
#new_map %>% filter(NCIT == "C128799")

create_synapse_table_version(table_id = synid_table_map, 
                             data = new_map, 
                             comment = "get missing NCIT codes", 
                             append = F)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
