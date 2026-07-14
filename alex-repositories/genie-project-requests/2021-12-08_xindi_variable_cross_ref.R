# Description: Cross reference cohort specific variables and missing data variables.
# Author: Haley Hunter-Zinck
# Date: 2021-12-08

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_empty <- "syn26467720"
synid_folder_output <- "syn26162727"

# param
vars_specific_name <- c()
vars_empty <- c()

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

vars_empty <- as.character(unlist(get_synapse_entity_data_in_csv(synid_file_empty)))

# main ----------------------------

vars_specific_name <- c("ca_crc_trg", "ca_crc_kras", "rt_phases")
for (i in 31:45) {
  vars_specific_name <- append(vars_specific_name, glue("path_ca_ishist{i}"))
}
for (i in 1:3) {
  vars_specific_name <- append(vars_specific_name, glue("rt_type_p{i}"))
}
for (i in 1:3) {
  vars_specific_name <- append(vars_specific_name, glue("rt_planning_p{i}"))
}
for (i in 1:3) {
  vars_specific_name <- append(vars_specific_name, glue("rt_dose_p{i}"))
}
for (i in 1:3) {
  vars_specific_name <- append(vars_specific_name, glue("rt_site_p{i}"))
}
vars_specific_cohort <- c(rep("NSCLC", 2), rep("BrCa", 28))

res <- data.frame(var_name = vars_specific_name, cohort = vars_specific_cohort) %>%
  mutate(is_empty = is.element(var_name, vars_empty)) 

# write --------------------------------

file_res <- "2021-12-08_cohort_specific_variables.csv"
write.csv(res, file = file_res, row.names = F)

save_to_synapse(path = file_res, 
                parent_id = synid_folder_output, 
                prov_name = "BPC cohort specific variables", 
                prov_desc = "cross reference BPC cohort specific variables with list of variables that are empty in the synapse tables", 
                prov_used = synid_file_empty, 
                prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2021-12-08_xindi_variable_cross_ref.R")

file.remove(file_res)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
