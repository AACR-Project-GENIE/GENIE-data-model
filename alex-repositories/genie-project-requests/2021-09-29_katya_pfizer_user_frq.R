# Description: Determine if I can access Synapse user login information.  
# Author: Haley Hunter-Zinck
# Date: 2021-09-29

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# files
file_users <- "user_names.txt"
project_names <- c("GENIE BioPharma Collaborative")
file_output <- "2021-09-29_pfizer_user_synapse_bpc_last_activity.csv"

# synapse
synid_folder_output <- "syn26263015"
synid_file_user_names <- "syn26263016"

# functions ----------------------------

#' Get the user Synapse ID from the user's Synapse user name.
#' 
#' @param user_name Synapse user name
#' @return Synapse user ID number
#' @example get_user_id("hhz")
get_user_id <- function(user_name) {
  return(synGetUserProfile(user_name)$ownerId)
}

get_project_last_activity <- function(user_name, project_name) {
  
  user_id <- get_user_id(user_name)
  
  if (is.na(user_id)) {
    return(NA)
  }
  
  access <- synRestGET(glue("/projects/user/{user_id}"))
  
  project_names <- unlist(lapply(access$results, function(x) {return(x$name)}))
  
  if (is.element(project_name, project_names)) {
    la <- access$results[[which(project_names == project_name)]]$lastActivity
    return(as.character(as.POSIXct(la, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")))
  }
  
  return(NA)
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

get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           sep = ",", 
                                           na.strings = c("NA"),
                                           header = F) {
  
  data <- read.csv(synGet(synapse_id)$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, header = header)
  return(data)
}


# read ----------------------------

user_names <- get_synapse_entity_data_in_csv (synid_file_user_names)
user_names <- as.character(unlist(user_names))

# main ----------------------------

last_activity <- matrix(NA, nrow = length(user_names), 
                        ncol = length(project_names),
                        dimnames = list(user_names, project_names))

# access date
for (user_name in user_names) {
  for (project_name in project_names) {
    last_activity[user_name, project_name] <- get_project_last_activity(user_name = user_name,
                                               project_name = project_name)
  }
}

# write to file ------------------------

write.csv(last_activity, row.names = T, 
          file = file_output)
save_to_synapse(path = file_output, 
                parent_id = synid_folder_output, 
                prov_name = "last activity", 
                prov_desc = "last activity for BPC project for list of user names", 
                prov_used = synid_file_user_names, 
                prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2021-09-29_katya_pfizer_user_frq.R")
file.remove(file_output)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
