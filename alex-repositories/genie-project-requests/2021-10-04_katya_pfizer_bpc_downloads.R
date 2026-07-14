# Description: 
# Author: Haley Hunter-Zinck
# Date: 

# setup ----------------------------

tic = as.double(Sys.time())

library(DBI)
library(glue)
library(dplyr)
library(yaml)
library(synapseusagereports)
library(synapser)
synLogin()

# synapse
synid_file_users <- "syn26263016"
synid_project_bpc <- "syn21226493"
synid_folder_request <- "syn26263015"
synid_folder_brca <- "syn24981907"
synid_folder_crc <- "syn23561875"
synid_folder_nsclc <- "syn21459571"

# parameters
config_file <- "db-config.yml"
query_type <- "filedownloadrecord"
#start_date <- "2021-04-01"
start_date <- "2021-01-01"
end_date <- "2021-10-04"

# functions ----------------------------

#' Get the user Synapse ID from the user's Synapse user name.
#' 
#' @param user_name Synapse user name
#' @return Synapse user ID number
#' @example get_synapse_user_id("hhz")
get_synapse_user_id <- function(user_name) {
  return(synGetUserProfile(user_name)$ownerId)
}

get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA,
                                           sep = ",", 
                                           na.strings = c("NA"), 
                                           header = T) {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = F,
                   header = header)
  return(data)
}

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

#' Depth first search for tree traversal of root folder directory.
#' 
#' @param synid_root Synapse ID of the root folder/project.
#' @return Vector of Synapse IDs of all descendants of the root 
#' @example 
#' traverse("syn12345")
traverse <- function(synid_root) {
  
  synid_folders <- c()
  synid_children <- as.list(synGetChildren(synid_root, includeTypes = list("folder", "file")))
  
  if(!length(synid_children)) {
    return(synid_root)
  }
  
  for (synid_child in synid_children) {
    synid_folders <- append(synid_folders, traverse(synid_child$id))
  }
  
  return(c(synid_root, synid_folders))
}

# connect to database ---------------

config <- yaml.load_file(config_file)

con <- RMySQL::dbConnect(RMySQL::MySQL(),
                         user = config$username,
                         password = config$password,
                         host = config$host,
                         dbname = config$db)


# read -----------------------------

user_names <- as.character(unlist(get_synapse_entity_data_in_csv(synid_file_users, header = F)))

# synapse folder traverses to infer cohort for files
brca <- traverse(synid_folder_brca)
crc <- traverse(synid_folder_crc)
nsclc <- traverse(synid_folder_nsclc)

# get downloads for each user
query_bpc <- report_data_query(con = con, 
                               project_id = synid_project_bpc,
                               query_type = query_type,
                               start_date = start_date,
                               end_date = end_date)

# main ----------------------------

# get all user ids
user_ids <- c()
for (user_name in user_names) {
  user_ids[user_name] <- get_synapse_user_id(user_name)
}
user_names <- setNames(user_names, user_ids)

# get downloads for pfizer user ids
query_pfizer <- query_bpc %>%
  filter(userId %in% user_ids) %>%
  mutate(user_name = user_names[userId]) %>%
  mutate(synapse_id = paste0("syn", id)) %>%
  mutate(cohort = case_when(is.element(synapse_id, brca) ~ "BrCa",
                            is.element(synapse_id, crc) ~ "CRC",
                            is.element(synapse_id, nsclc) ~ "NSCLC",
                            TRUE ~ "GENIE")) %>%
  rename(file_name = NAME) %>%
  select(user_name, date, cohort, file_name, synapse_id) %>%
  arrange(user_name, date, cohort, file_name)
  
summ_pfizer <- query_pfizer %>%
  group_by(user_name, cohort) %>%
  count() %>%
  rename(n_downloads = n) %>%
  select(user_name, cohort, n_downloads) %>%
  arrange(user_name, cohort)

# write --------------------------------

file_bpc <- glue("{start_date}_{end_date}_{query_type}_bpc_all.csv")
write.csv(query_bpc, row.names = F, file = file_bpc)
file_all <- glue("{start_date}_{end_date}_{query_type}_pfizer_all.csv")
write.csv(query_pfizer, row.names = F, file = file_all)
file_summ <- glue("{start_date}_{end_date}_{query_type}_pfizer_summary.csv")
write.csv(summ_pfizer, row.names = F, file = file_summ)

save_to_synapse(path = file_all, 
                parent_id = synid_folder_request,
                prov_name = "Pfizer user BPC downloads", 
                prov_desc = "All downloads from 2021-07-01 to 2021-10-04 for Pfizer users from the BPC project", 
                prov_used = synid_file_users, 
                prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2021-10-04_katya_pfizer_bpc_downloads.R")
save_to_synapse(path = file_summ, 
                parent_id = synid_folder_request, 
                prov_name = "Pfizer user BPC download summary", 
                prov_desc = "Summary of downloads from 2021-07-01 to 2021-10-04 for Pfizer users from the BPC project", 
                prov_used = synid_file_users, 
                prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2021-10-04_katya_pfizer_bpc_downloads.R")

# close out ----------------------------

# clean up
file.remove(file_all)
file.remove(file_summ)
file.remove(file_bpc)

# disconnect from database
dbDisconnect(con)

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
