# Description: combine BPC data files from multiple cohorts.
# Author: Xindi Guo
# Date: 2021-12-15

# setup ----------------------------

tic = as.double(Sys.time())

library(data.table)
library(dplyr)
library(glue)
library(synapser)
synLogin()

# synapse
synid_folder_crc <- "syn26046784"
synid_folder_nsclc <- "syn25982471"
synid_folder_brca <- "syn26253353"
synid_file_selected_ids <- "syn26529353"
synid_folder_output <- "syn26529348"

# functions ----------------------------

#' Download files from a given folder on Synapse
#'
#' @param folder_id Synapse ID of the folder
#' @return list of named dataframes
download_files_from_folder <- function(folder_id) {
  ls_files <- synGetChildren(folder_id)$asList()
  df_files <- do.call(rbind.data.frame, ls_files)
  df_files <- df_files %>% 
    remove_rownames %>% 
    column_to_rownames(var="name")
  ls_df_dat <- apply(df_files, 1, function(x){
    fread(synGet(x['id'])$path)
  })
  return(ls_df_dat)
}

#' Filter the data by given record ID and cohort
#'
#' @param dat list of dataframes
#' @param df_record_id dataframe of cohort and record ids
#' @param cohort selected cancer cohort
#' @return filtered list of dataframes
filter_dat_by_id <- function(dat, 
                             df_record_ids, 
                             cohort) {
  selected_record_ids <- df_record_ids %>% 
    filter(cohort==cohort) %>% 
    select(record_id) %>% 
    pull(1)
  filtered_ls_df_dat <- lapply(dat, function(x){
    filter(x,record_id %in% selected_record_ids)
  })
  return(filtered_ls_df_dat)
}

#' Combine the list of dataframes
#'
#' @param ls_df list of dataframes
#' @param name name in the list
#' @return combined dataframe
combine_df_by_name <- function(ls_df, 
                               name) {
  filtered_ls_df <- lapply(ls_df,function(x){
    x[[name]]
  })
  combined_df <- do.call(plyr::rbind.fill, filtered_ls_df)
  return(combined_df)
}

#' Helper function from Haley: Store a file on Synapse with options to define provenance.
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
helper_save_to_synapse <- function(path, 
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

#' Store the file to Synpase
#' 
#' @param dat Data to store
#' @param path Path to the file on the local machine.
#' @param parent_id Synapse ID of the folder or project to which to load the file.
#' @return Synapse ID of entity representing file
save_to_synapse <- function(dat, 
                            path,
                            parent_id){
  write.csv(dat, file = path, row.names = F, quote = T, na = "")
  helper_save_to_synapse(path = path, 
                         parent_id = synid_folder_output,
                         prov_name = "merged data by selected ids", 
                         prov_desc = "data merged by selected ids from BrCa, CRC, and NSCLC cohorts", 
                         prov_used = c(synid_file_selected_ids, synid_folder_brca, synid_folder_crc, synid_folder_nsclc), 
                         prov_exec = "https://github.com/xindiguo/genie_requests/blob/main/2021-12-15_joyce_combine_data_from_bpc.R")
  file.remove(path)
}

# read ----------------------------

selected_ids <- fread(synGet(synid_file_selected_ids)$path)
dat_crc <- download_files_from_folder(synid_folder_crc)
dat_brca <- download_files_from_folder(synid_folder_brca)
dat_nsclc <- download_files_from_folder(synid_folder_nsclc)

# main ----------------------------

filtered_dat_crc <- filter_dat_by_id(dat_crc, selected_ids, "CRC")
filtered_dat_brca <- filter_dat_by_id(dat_brca, selected_ids, "BrCa")
filtered_dat_nsclc <- filter_dat_by_id(dat_nsclc, selected_ids, "NSCLC")

dat_ca_index <- combine_df_by_name(list(filtered_dat_crc,
                                        filtered_dat_brca,
                                        filtered_dat_nsclc),
                                   "cancer_level_dataset_index.csv")
dat_ca_non_index <- combine_df_by_name(list(filtered_dat_crc,
                                            filtered_dat_brca,
                                            filtered_dat_nsclc),
                                       "cancer_level_dataset_non_index.csv")
dat_cpt <- combine_df_by_name(list(filtered_dat_crc,
                                   filtered_dat_brca,
                                   filtered_dat_nsclc),
                              "cancer_panel_test_level_dataset.csv")
dat_image <-combine_df_by_name(list(filtered_dat_crc,
                                    filtered_dat_brca,
                                    filtered_dat_nsclc),
                               "imaging_level_dataset.csv")          
dat_md <- combine_df_by_name(list(filtered_dat_crc,
                                  filtered_dat_brca,
                                  filtered_dat_nsclc),
                             "med_onc_note_level_dataset.csv")   
dat_path <- combine_df_by_name(list(filtered_dat_crc,
                                    filtered_dat_brca,
                                    filtered_dat_nsclc),
                               "pathology_report_level_dataset.csv")
dat_pt <- combine_df_by_name(list(filtered_dat_crc,
                                  filtered_dat_brca,
                                  filtered_dat_nsclc),
                             "patient_level_dataset.csv")       
dat_drug <- combine_df_by_name(list(filtered_dat_crc,
                                    filtered_dat_brca,
                                    filtered_dat_nsclc),
                               "regimen_cancer_level_dataset.csv")
dat_tm <- combine_df_by_name(list(filtered_dat_crc,
                                  filtered_dat_brca),
                             "tm_level_dataset.csv")

# write ---------------------------

save_to_synapse(dat_ca_index, "cancer_level_dataset_index.csv", synid_folder_output)
save_to_synapse(dat_ca_non_index,"cancer_level_dataset_non_index.csv", synid_folder_output)
save_to_synapse(dat_cpt, "cancer_panel_test_level_dataset.csv", synid_folder_output)
save_to_synapse(dat_image, "imaging_level_dataset.csv", synid_folder_output)
save_to_synapse(dat_md,"med_onc_note_level_dataset.csv", synid_folder_output)
save_to_synapse(dat_path, "pathology_report_level_dataset.csv", synid_folder_output)
save_to_synapse(dat_pt, "patient_level_dataset.csv", synid_folder_output)
save_to_synapse(dat_drug, "regimen_cancer_level_dataset.csv", synid_folder_output)
save_to_synapse(dat_tm, "tm_level_dataset.csv", synid_folder_output)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
