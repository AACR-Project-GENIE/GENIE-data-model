# Description: combine BPC variable synopsis from multiple cohorts.
# Author: Haley Hunter-Zinck
# Date: 2021-12-13

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_crc <- "syn26077307"
synid_file_nsclc <- "syn26028547"
synid_file_brca <- "syn26077309"
synid_folder_output <- "syn26529348"

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

synop_crc <- get_synapse_entity_data_in_xlsx(synapse_id = synid_file_crc,
                                             sheet = 2)
synop_nsclc <- get_synapse_entity_data_in_xlsx(synapse_id = synid_file_nsclc,
                                             sheet = 2)
synop_brca <- get_synapse_entity_data_in_xlsx(synapse_id = synid_file_brca,
                                             sheet = 2)


# main ----------------------------

synop_all <- bind_rows(bind_rows(synop_brca, synop_crc), synop_nsclc)
idx_dup <- which(duplicated(synop_all$Variable.Name))

synop_all <- synop_all[-idx_dup, ] %>%
  mutate(BrCa = is.element(Variable.Name, synop_brca$Variable.Name)) %>%
  mutate(CRC = is.element(Variable.Name, synop_crc$Variable.Name)) %>%
  mutate(NSCLC = is.element(Variable.Name, synop_nsclc$Variable.Name)) %>%
  select(Dataset, Variable.Name, Field.Label, Data.Type, Values, BrCa, CRC, NSCLC)

# write ----------------------------

file_output <- "BPC-consortium_variables_synopsis.xlsx"
write.xlsx(synop_all, file = file_output, overwrite = T)

save_to_synapse(path = file_output, 
                parent_id = synid_folder_output,
                prov_name = "merged variable synopsis", 
                prov_desc = "variable synopsis from merging variable synopsis from BrCa, CRC, and NSCLC cohorts", 
                prov_used = c(synid_file_brca, synid_file_crc, synid_file_nsclc), 
                prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2021-12-13_xindi_combine_variable_synopsis.R")


file.remove(file_output)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
