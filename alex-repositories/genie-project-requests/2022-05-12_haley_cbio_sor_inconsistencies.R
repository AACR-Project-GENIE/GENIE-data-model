# Description: Get actual value of release in SOR for variables
#   flagged as inconsistent between the SOR and BPC cBio mapping file.
# Author: Haley Hunter-Zinck
# Date: 2022-05-12

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)

# synapse
synid_file_sor <- "syn22294851"
synid_file_brca <- "syn30134386"
synid_file_crc <- "syn30095793"
synid_table_rel <- "syn27628075"
synid_folder_output <- "syn25931947"

# parameters
coi_no <- 5
entity_name <- "sor_cbio_inconistencies.csv"
outfile <- glue("{Sys.Date()}_{entity_name}")

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
                                            check.names = F,
                                            sep.names = " ") {
  library(openxlsx)
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.xlsx(entity$path, check.names = check.names, sheet = sheet, sep.names = sep.names)
  
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

df_sor <- get_synapse_entity_data_in_xlsx(synid_file_sor, sheet = 2)
df_brca <- get_synapse_entity_data_in_csv(synid_file_brca)
df_crc <- get_synapse_entity_data_in_csv(synid_file_crc)

query <- glue("SELECT sor_cbio_column FROM {synid_table_rel} WHERE cohort = 'BrCa' AND release_version = '1.2' AND release_type = 'consortium'")
col_sor_brca <- as.character(unlist(as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))))

query <- glue("SELECT sor_cbio_column FROM {synid_table_rel} WHERE cohort = 'CRC' AND release_version = '2.0' AND release_type = 'public'")
col_sor_crc <- as.character(unlist(as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))))


# main ----------------------------

var_brca <- as.character(unlist(df_brca %>%
  filter(check_no == coi_no) %>%
  select(code)))

var_crc <- as.character(unlist(df_crc %>%
                                 filter(check_no == coi_no) %>%
                                 select(code)))

sor_val <- df_sor %>% 
  filter(is.element(VARNAME, unique(var_brca, var_crc))) %>%
  select(VARNAME, col_sor_brca, col_sor_crc)
print(sor_val)

sor_val_write <- sor_val %>%
  select(VARNAME, col_sor_brca)

# write -------------------------------

towrite <- write.csv(sor_val_write, file = outfile, row.names = F, na = "")

save_to_synapse(path = outfile, 
                parent_id = synid_folder_output, 
                file_name = entity_name, 
                prov_name = "SOR versus cbio mapping", 
                prov_desc = "Get actual values for scope of release versus cBioPortal mapping", 
                prov_used = c(synid_file_sor, synid_file_brca, synid_file_crc), 
                prov_exec = "https://github.com/Sage-Bionetworks/genie-project-requests/blob/main/2022-05-12_haley_cbio_sor_inconsistencies.R")

file.remove(outfile)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
