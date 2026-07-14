# Description: examine user access in folders in the GENIE BPC Internal project to
#   determine the possibility of using a team to control access for the QA managers 
#   and other roles rather than granting permissions on a user level.
# Author: Haley Hunter-Zinck
# Date: August 11, 2021

# setup -------------------------------------

tic = as.double(Sys.time())

library(synapser)
synLogin()
library(glue)
library(reshape)

# synapse IDs
synid_project <- "syn21435590"  # GENIE BioPharma Collaborative Internal
name_team_bpc <- "Project GENIE BioPharma Collaborative Internal Team" 
name_team_qa <- "GENIE BPC QA Managers"

# files
outfile <- "bpc_permissions.csv"

# constants
MAP_PERMISSIONS <- setNames(c("Can view", "Can download", "Can edit", "Can edit & delete", "Administrator"),
                            c("READ", "DOWNLOAD,READ", "CREATE,DOWNLOAD,READ,UPDATE", 
                                "CREATE,DELETE,DOWNLOAD,READ,UPDATE","CHANGE_PERMISSIONS,CHANGE_SETTINGS,CREATE,DELETE,DOWNLOAD,MODERATE,READ,UPDATE"))

# functions  -------------------------------------

# determine if synapse ID represents a folder
is_folder <- function(synid) {
  
  ent <- synGet(synid)
  return(as.logical(length(grep(pattern = "Folder", x = ent$properties$concreteType))))
}

# determine if synapse ID represents a project
is_project <- function(synid) {
  
  ent <- synGet(synid)
  return(as.logical(length(grep(pattern = "Project", x = ent$properties$concreteType))))
}

# depth first search for tree traversal of root folder directory
traverse <- function(synid_root) {
  
  synid_folders <- c()
  synid_children <- as.list(synGetChildren(synid_root, includeTypes = list("folder")))
  
  if(!length(synid_children)) {
    return(synid_root)
  }
  
  for (synid_child in synid_children) {
    synid_folders <- append(synid_folders, traverse(synid_child$id))
  }
  
  return(synid_folders)
}

get_user_names <- function(name_team_bpc) {
  
  user_names <- c()
  my_list <- as.list(synGetTeamMembers(team = synGetTeam(name_team_bpc)))
  
  for(item in my_list) {
    user_names <- append(user_names, item$member$userName)
  }
  
  return(user_names)
}

get_user_permissions <- function(pair, collapse = T, map = T, default = NA) {
  
  synid <- as.character(pair["synid"])
  user_name <- as.character(pair["user_name"])
  permissions <- unlist(synGetPermissions(synid, user_name))
  
  if(!is.na(default) & length(permissions) == 0) {
    permissions = default
  }
  
  if(collapse) {
    permissions <- paste0(sort(permissions), collapse = ",")
    
    if(map) {
      permissions <- MAP_PERMISSIONS[permissions]
    }
  }
  
  return(permissions)
}

get_user_folder_matrix <- function(users, synids) {
  
  permissions <- matrix(NA, nrow = length(synids), ncol = length(users), 
                        dimnames = list(synids, users_all))
  
  pairs <- expand.grid(synids, users, stringsAsFactors = F)
  names(pairs) <- c("synid", "user_name")
  pairs_permissions <- apply(pairs, 1, get_user_permissions)
  permissions <- matrix(pairs_permissions, nrow = length(synids), byrow = F, dimnames = list(synids, users))
  
  return(permissions)
}

# get users and folders ---------------

# get all members of all teams
users_team_bpc <- get_user_names(name_team_bpc)
synid_folders <- traverse(synid_project)

# get permissions -------------------------------------

users_all <- c(users_team_bpc)

# get permissions for each folder-user pair

write.csv(permissions, file = outfile)

# analyze qa manager patterns -------------------------------------

users_qa <- c("celeste.yu", "hira.rizvi", "apostle", "Michele")
raw <- read.csv(outfile, stringsAsFactors = F)
per <- raw[,2:ncol(raw)]
rownames(per) <- raw[,1]
cbind(per[users_qa], per[1])

# close out -------------------------------------

toc = as.double(Sys.time())
print(glue("Outfile: {outfile}"))
print(glue("Runtime: {round(toc - tic)} s"))
