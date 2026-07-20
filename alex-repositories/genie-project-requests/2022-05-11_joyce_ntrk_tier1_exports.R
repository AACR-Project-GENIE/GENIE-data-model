# Description: Get tier 1 exports for the GENIE NTRK Sponsored Project using
#   the most recent GENIE 12.1-consortium release used in previous export.
# Adapted from: https://github.com/Sage-Bionetworks/Genie_processing/blob/master/redcap/NTRK_export_selected_cases.R
# Author: Haley Hunter-Zinck
# Date: 2022-05-11

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(sqldf)
library(synapser)

# synapse
synid_file_ids <- "syn26529362"
synid_file_sam <- "syn9734573"
synid_version_sam <- 196
synid_file_pat <- "syn9734568"
synid_version_pat <- 186
synid_table_sex <- "syn7434222"
synid_table_race <- "syn7434236"
synid_table_ethn <- "syn7434242"
synid_table_sty <- "syn7434273"
synid_folder_output <- "syn26126365"

# parameters
site = "MSK"
output_entity_name <- glue("NTRK_selected_cases_{site}.csv")
output_file_name <- glue("NTRK_selected_cases_{site}_{Sys.Date()}.csv")

# functions ----------------------------

#' Extract personal access token from .synapseConfig
#' located at a custom path. 
#' 
#' @param path Path to .synapseConfig
#' @return personal acccess token
get_auth_token <- function(path) {
  
  lines <- scan(path, what = "character", sep = "\t", quiet = T)
  line <- grep(pattern = "^authtoken = ", x = lines, value = T)
  
  token <- strsplit(line, split = ' ')[[1]][3]
  return(token)
}

#' Override of synapser::synLogin() function to accept 
#' custom path to .synapseConfig file or personal authentication
#' token.  If no arguments are supplied, performs standard synLogin().
#' 
#' @param auth full path to .synapseConfig file or authentication token
#' @param silent verbosity control on login
#' @return TRUE for successful login; F otherwise
synLogin <- function(auth = NA, silent = T) {
  
  secret <- Sys.getenv("SCHEDULED_JOB_SECRETS")
  if (secret != "") {
    # Synapse token stored as secret in json string
    syn = synapser::synLogin(silent = T, authToken = fromJSON(secret)$SYNAPSE_AUTH_TOKEN)
  } else if (auth == "~/.synapseConfig" || is.na(auth)) {
    # default Synapse behavior
    syn <- synapser::synLogin(silent = silent)
  } else {
    
    # in case pat passed directly
    token <- auth
    
    # extract token from custom path to .synapseConfig
    if (grepl(x = auth, pattern = "\\.synapseConfig$")) {
      token = get_auth_token(auth)
      
      if (is.na(token)) {
        return(F)
      }
    }
    
    # login with token
    syn <- tryCatch({
      synapser::synLogin(authToken = token, silent = silent)
    }, error = function(cond) {
      return(F)
    })
  }
  
  # NULL returned indicates successful login
  if (is.null(syn)) {
    return(T)
  }
  return(F)
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

# synapse login --------------------

status <- synLogin()

# read ----------------------------

# mapping tables
sex_mapping <- synTableQuery(glue("SELECT * FROM {synid_table_sex}"))$asDataFrame()
race_mapping <- synTableQuery(glue("SELECT * FROM {synid_table_race}"))$asDataFrame()
ethnicity_mapping <- synTableQuery(glue("SELECT * FROM {synid_table_ethn}"))$asDataFrame()
sample_type_mapping <- synTableQuery(glue("SELECT * FROM {synid_table_sty}"))$asDataFrame()

# read selected patient and sample IDs
selected_info <- get_synapse_entity_data_in_xlsx(synid_file_ids)

# main ----------------------------

# patient patient and sample IDs requested
selected_cases <- grep(x = unique(selected_info$Patient.ID), pattern = site, value = T)
selected_samples <- grep(x = unique(selected_info$Sample.ID), pattern = site, value = T)

# Create query for selected cases
temp <- toString(selected_samples)
temp <- sapply(strsplit(temp, '[, ]+'), function(x) toString(shQuote(x)))

# download clinical data
# sample clinical data
clinical_sample <- read.delim(synGet(synid_file_sam, downloadFile = TRUE, followLink = TRUE, version = synid_version_sam)$path, skip = 4, header = TRUE)
clinical_sample <- sqldf(paste("SELECT * FROM clinical_sample where SAMPLE_ID in (",temp,")",sep = ""))

# patient clinical data
clinical_patient <- read.delim(synGet(synid_file_pat, downloadFile = TRUE, followLink = TRUE, version = synid_version_pat)$path, skip = 4, header = TRUE)

# combined clinical data
sql <- "select * from clinical_sample left join clinical_patient on clinical_sample.PATIENT_ID = clinical_patient.PATIENT_ID"
clinical <- sqldf(sql)

# check the patients
table(unique(clinical$PATIENT_ID) %in% selected_cases)

# change the columns to lower case
colnames(clinical) <- tolower(colnames(clinical))

# Get all samples for those patients
samples_per_patient <- sapply(unique(clinical$patient_id), function(x){as.character(clinical$sample_id[clinical$patient_id %in% x])})

# mapping data for each instrument
# instrument - patient_characteristics
patient_output <- data.frame("record_id" = unique(clinical$patient_id))
patient_output$redcap_repeat_instrument <- rep("")
patient_output$redcap_repeat_instance <- rep("")
patient_output$redcap_data_access_group <- clinical$center[match(patient_output$record_id, clinical$patient_id)]

patient_output$genie_patient_id <- patient_output$record_id
patient_output$birth_year <- clinical$birth_year[match(patient_output$genie_patient_id, clinical$patient_id)]
patient_output$naaccr_ethnicity_code <- clinical$ethnicity[match(patient_output$genie_patient_id, clinical$patient_id)]
patient_output$naaccr_race_code_primary <- clinical$primary_race[match(patient_output$genie_patient_id, clinical$patient_id)]
patient_output$naaccr_race_code_secondary <- clinical$secondary_race[match(patient_output$genie_patient_id, clinical$patient_id)]
patient_output$naaccr_race_code_tertiary <- clinical$tertiary_race[match(patient_output$genie_patient_id, clinical$patient_id)]
patient_output$naaccr_sex_code <- clinical$sex[match(patient_output$genie_patient_id, clinical$patient_id)]

# mapping to code
patient_output$naaccr_ethnicity_code <- ethnicity_mapping$CODE[match(patient_output$naaccr_ethnicity_code, ethnicity_mapping$CBIO_LABEL)]
patient_output$naaccr_race_code_primary <- race_mapping$CODE[match(patient_output$naaccr_race_code_primary, race_mapping$CBIO_LABEL)]
patient_output$naaccr_race_code_secondary <- race_mapping$CODE[match(patient_output$naaccr_race_code_secondary, race_mapping$CBIO_LABEL)]
patient_output$naaccr_race_code_tertiary <- race_mapping$CODE[match(patient_output$naaccr_race_code_tertiary, race_mapping$CBIO_LABEL)]
patient_output$naaccr_sex_code <- sex_mapping$CODE[match(patient_output$naaccr_sex_code,sex_mapping$CBIO_LABEL)]

# recode
# cannotReleaseHIPAA = NA
patient_output$birth_year[which(patient_output$birth_year == "cannotReleaseHIPAA")] <- NA
# withheld = NA
patient_output$birth_year[which(patient_output$birth_year == "withheld")] <- NA
# -1 Not collected = 9 Unknown
patient_output$naaccr_ethnicity_code[which(patient_output$naaccr_ethnicity_code == -1)] <- 9
# -1 Not collected = 99 Unknown
patient_output$naaccr_race_code_primary[which(patient_output$naaccr_race_code_primary == -1)] <- 99
# -1 Not collected = 88 according to NAACCR
patient_output$naaccr_race_code_secondary[which(patient_output$naaccr_race_code_secondary == -1)] <- 88
patient_output$naaccr_race_code_tertiary[which(patient_output$naaccr_race_code_tertiary == -1)] <- 88


# instrument - cancer_panel_test
sample_info_list <- lapply(samples_per_patient,function(x){
  sample_list = list()
  for(i in 1:length(x)){
    temp_df = data.frame("record_id" = clinical$patient_id[clinical$sample_id == x[i]])
    temp_df$redcap_repeat_instrument = "cancer_panel_test"
    temp_df$redcap_repeat_instance = i
    temp_df$redcap_data_access_group = clinical$center[clinical$sample_id == x[i]]
    
    temp_df$cpt_genie_sample_id = x[i]
    temp_df$cpt_oncotree_code = clinical$oncotree_code[clinical$sample_id == x[i]]
    temp_df$cpt_sample_type = clinical$sample_type_detailed[clinical$sample_id == x[i]]
    temp_df$cpt_seq_assay_id = clinical$seq_assay_id[clinical$sample_id == x[i]]
    temp_df$cpt_seq_date = clinical$seq_year[clinical$sample_id == x[i]]
    temp_df$age_at_seq_report = clinical$age_at_seq_report_days[clinical$sample_id == x[i]]
    sample_list[[i]] <- temp_df
  }
  combined_df = rbind.fill(sample_list)
  return(combined_df)
})

sample_info_df <- rbind.fill(sample_info_list)
patient_output <- rbind.fill(patient_output,sample_info_df)
patient_output$redcap_data_access_group <- NULL

# write ---------------------------------

write.csv(patient_output,file = output_file_name, quote = TRUE,row.names = FALSE,na = "")

synid_file_output <- save_to_synapse(path = output_file_name, 
                            parent_id = synid_folder_output, 
                            file_name = output_entity_name, 
                            prov_name = "export main GENIE data", 
                            prov_desc = "Export selected BPC patient data from main GENIE database", 
                            prov_used = c(synid_file_ids, synid_file_pat, synid_file_sam), 
                            prov_exec = "https://github.com/Sage-Bionetworks/genie-project-requests/blob/main/2022-05-11_joyce_ntrk_tier1_exports.R")

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
