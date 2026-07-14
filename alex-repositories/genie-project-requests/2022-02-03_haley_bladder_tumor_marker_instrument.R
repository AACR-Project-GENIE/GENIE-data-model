# Description: Check for tumor_marker instrument in BPC BLADDER uploads. 
# Author: Haley Hunter-Zinck
# Date: 2022-02-03

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_dfci_data <- "syn26250078"
synid_file_dfci_header <- "syn26250090"
synid_file_msk <- "syn26250083"
synid_file_uhn <- "syn26250084"
synid_file_vicc <- "syn26250085"

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

# read ----------------------------

dfci <- get_synapse_entity_data_in_csv(synid_file_dfci_data)
colnames(dfci) <- get_synapse_entity_data_in_csv(synid_file_dfci_header)
msk <- get_synapse_entity_data_in_csv(synid_file_msk)
uhn <- get_synapse_entity_data_in_csv(synid_file_uhn)
vicc <- get_synapse_entity_data_in_csv(synid_file_vicc)

# main ----------------------------

n_dfci <- dfci %>% 
  filter(redcap_repeat_instrument == 'prissmm_tumor_marker') %>%
  count()
n_msk <- msk %>% 
  filter(redcap_repeat_instrument == 'prissmm_tumor_marker') %>%
  count()
n_uhn <- uhn %>% 
  filter(redcap_repeat_instrument == 'prissmm_tumor_marker') %>%
  count()
n_vicc <- vicc %>% 
  filter(redcap_repeat_instrument == 'prissmm_tumor_marker') %>%
  count()

print(dfci %>% select(redcap_repeat_instrument) %>% distinct() %>% arrange(redcap_repeat_instrument))

# close out ----------------------------

print(glue("Number of 'prissmm_tumor_marker' instances for DFCI: {n_dfci}"))
print(glue("Number of 'prissmm_tumor_marker' instances for MSK: {n_msk}"))
print(glue("Number of 'prissmm_tumor_marker' instances for UHN: {n_uhn}"))
print(glue("Number of 'prissmm_tumor_marker' instances for VICC: {n_vicc}"))

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
