# Description: Look into upload errors for COLU.  
# Author: Haley Hunter-Zinck
# Date: 2022-01-26

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_maf <- "syn17227340"
synid_ver_maf <- "33"
synid_file_pat <- "syn18693147"
synid_ver_pat <- "22"
synid_folder_output <- "syn11703487"

# parameters
outfile <- "COLU_errors_details.csv"

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

maf <- get_synapse_entity_data_in_csv(synid_file_maf, 
                               version = synid_ver_maf,
                               sep = "\t")
pat <- get_synapse_entity_data_in_csv(synid_file_pat, 
                                      version = synid_ver_pat,
                                      sep = "\t")

# main: vital status errors ----------------------------

# inconsistent redaction values in YEAR_CONTACT, INT_CONTACT.
pat_red_err <- pat %>%
  filter(grepl(pattern = "[<>]", x = YEAR_DEATH) & !grepl(pattern = "[<>]", x = INT_DOD) |
           !grepl(pattern = "[<>]", x = YEAR_DEATH) & grepl(pattern = "[<>]", x = INT_DOD)) %>%
  mutate(synapse_id = glue("{synid_file_pat}.{synid_ver_pat}")) %>%
  mutate(check = 1) %>%
  mutate(error = 'Redaction is inconsistent between YEAR_DEATH and INT_DOD') %>%
  mutate(request = 'Please redact both or neither value') %>%
  select(synapse_id, PATIENT_ID, YEAR_DEATH, INT_DOD, DEAD, check, error, request)

# you have inconsistent redaction and text values in YEAR_DEATH, INT_DOD. 
strs <- c("Unknown", "Not Collected", "Not Applicable", "Not Released")
pat_err_text <- pat %>%
  filter((is.element(YEAR_DEATH, strs) | is.element(INT_DOD, strs)) & YEAR_DEATH != INT_DOD) %>%
  mutate(synapse_id = glue("{synid_file_pat}.{synid_ver_pat}")) %>%
  mutate(check = 2) %>%
  mutate(error = 'Numeric and text values are inconsistent between YEAR_DEATH and INT_DOD') %>%
  mutate(request = 'Please use numeric values for both or neither.') %>%
  select(synapse_id, PATIENT_ID, YEAR_DEATH, INT_DOD, DEAD, check, error, request)

# DEAD value is inconsistent with INT_DOD for at least one patient.
strs_true <- c("TRUE", "True")
pat_err_dead_na <- pat %>% 
  filter((INT_DOD == 'Not Applicable' | YEAR_DEATH == 'Not Applicable') & is.element(DEAD, strs_true)) %>%
  mutate(synapse_id = glue("{synid_file_pat}.{synid_ver_pat}")) %>%
  mutate(check = 3) %>%
  mutate(error = 'YEAR_DEATH and/or INT_DOD cannot be \'Not Applicable\' when DEAD is TRUE') %>%
  mutate(request = 'Please update the values to be consistent.') %>%
  select(synapse_id, PATIENT_ID, YEAR_DEATH, INT_DOD, DEAD, check, error, request)

# main: maf errors ----------------------------

maf_allele_err <- maf %>% 
  filter(Tumor_Seq_Allele1 != Reference_Allele) %>%
  mutate(synapse_id = glue("{synid_file_maf}.{synid_ver_maf}")) %>%
  mutate(check = 4) %>%
  mutate(error = 'Tumor_Seq_Allele1 does not match the Reference_Allele') %>%
  mutate(request = 'Please update Tumor_Seq_Allele1 or Reference_Allele to match.') %>%
  select(synapse_id, Tumor_Sample_Barcode, Reference_Allele, Tumor_Seq_Allele1, Tumor_Seq_Allele2, check, error, request)

print(maf_allele_err)

# write -------------------------------

to_write <- rbind(rbind(pat_red_err, pat_err_text), pat_err_dead_na) 
to_write <- to_write %>%
  mutate(issue = 1:nrow(to_write), .before = synapse_id) 
write.csv(to_write, file = outfile, row.names = F)

synid_file_output <- save_to_synapse(path = outfile, 
                parent_id = synid_folder_output, 
                prov_name = 'upload error details', 
                prov_desc = 'row based error reporting and details for issues in uploaded files', 
                prov_used = synid_file_pat, 
                prov_exec = 'https://github.com/hhunterzinck/genie_requests/blob/main/2022-01-26_marissa_upload_errors.R') 

file.remove(outfile)

# close out ----------------------------

print(glue("Written to '{outfile}' ({synid_file_output})"))

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
