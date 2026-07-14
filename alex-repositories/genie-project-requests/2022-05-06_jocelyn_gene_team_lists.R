# Description: Create team list files.  
# Author: Haley Hunter-Zinck
# Date: 2022-05-06

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)

# synapse
synid_user_genietm <- "SageGenieTM"
synid_folder_output <- "syn29817887"

# parameters
team_ids_exclude <- c("3386415", "3432639")
outfile <- "2022-05-06_genie_team_member_list.csv"

# functions ----------------------------

#' Determine if a Synapse user identifier is a valid user ID number (rather than
#' a user name string or non-existant ID number).
#' 
#' @param user Synapse user ID number of user name
#' @return TRUE is represents a valid user ID; FALSE otherwise
is_synapse_user_id <- function(user) {
  res <- tryCatch ({
    as.logical(length(synRestGET(glue("/user/{user}/bundle?mask=0x1"))))
  }, error = function(cond) {
    return(F)
  })
  
  return(res)
}

#' Get the user Synapse ID number from the user's Synapse user name.
#' 
#' @param user_name Synapse user name
#' @return Synapse user ID number
#' @example get_synapse_user_id("my_user_name")
get_synapse_user_id <- function(user_name) {
  
  if (is_synapse_user_id(user_name)) {
    return(user_name)
  }
  
  return(synGetUserProfile(user_name)$ownerId)
}


#' Get all the Synapse teams to which a user belongs.
#' @param user Synapse user ID number or user name.  
#' @return_ids If TRUE, return Synapse team ID numbers; otherwise return 
#' Synapse team names.
#' @return vector of Synapse team names or ID numbers; NA if user is neither
#' a valid Synapse user ID or user name.
get_synapse_user_teams <- function(user, return_ids = F) {
  
  team_names <- c()
  
  if (!is_synapse_user_id(user)) {
    user <- get_synapse_user_id(user)
  }
  
  if (is.na(user)) {
    return(NA)
  }
  
  team_entities <- synRestGET(glue("/user/{user}/team/id"))
  team_ids <- unlist(team_entities$teamIds)
  
  if(return_ids) {
    return(team_ids)
  }
  
  if(length(team_ids)) {
    for(team_id in team_ids) {
      team_names <- append(team_names, synGetTeam(team_id)$name)
    }
  }
  
  return(team_names)
}

#' Get the name of a Synapse team from the team ID.
#' 
#' @param team_id Synapse team ID number
#' @return String representing team name
#' @example get_synapse_team_name("12345")
#' @example get_synapse_team_name(c("12345", "234556"))
get_synapse_team_name <- function(team_id) {
  
  team_name <- rep(NA, length(team_id))
  for (i in 1:length(team_id)) {
    team_meta <- synRestGET(glue("/team/{team_id[i]}"))
    team_name[i] <- team_meta$name
  }
  
  return(team_name)
}

get_synapse_team_desc <- function(team_id) {
  
  team_desc <- rep(NA, length(team_id))
  for (i in 1:length(team_id)) {
    team_meta <- synRestGET(glue("/team/{team_id[i]}"))
    if (!is.null(team_meta$description) && team_meta$description != "") {
      team_desc[i] <- team_meta$description
    }
  }
  
  return(team_desc)
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

# main ----------------------------

# get team info 
team_ids <- setdiff(get_synapse_user_teams(synid_user_genietm, return_ids = T), team_ids_exclude)
team_names <- get_synapse_team_name(team_ids)
team_desc <- get_synapse_team_desc(team_ids)
team_info <- cbind(team_ids, team_names, team_desc)

# get individual team memebers
labels <- c("team_name", "team_description", "last_name", "first_name", "user_name", "team_id", "user_id")
member_info <- matrix(nrow = 0, ncol = length(labels), dimnames = list(c(), labels))
for (i in 1:length(team_ids)) {
  members <- as.list(synGetTeamMembers(team_ids[i]))
  for(member in members) {
    
    lastName <- ""
    if (!is.null(member$member$lastName)) {
      lastName <- member$member$lastName
    }
    
    firstName <- ""
    if (!is.null(member$member$firstName)) {
      firstName <- member$member$firstName
    }
    
    ind_info <- c(team_names[i],
                  team_desc[i], 
                  lastName,
                  firstName, 
                  member$member$userName, 
                  team_ids[i], 
                  member$member$ownerId)
    member_info <- rbind(member_info, ind_info)
  }
}

# write ----------------------

# write locally
write.csv(member_info, file = outfile, row.names = F)

# store to synpase
synid_file_output <- save_to_synapse (path = outfile, 
                  parent_id = synid_folder_output, 
                  prov_name = "Synapse GENIE Team Members", 
                  prov_desc = "List of Synapse GENIE team members for AACR review", 
                  prov_used = NA, 
                  prov_exec = "https://github.com/Sage-Bionetworks/genie-project-requests/blob/main/2022-05-06_jocelyn_gene_team_lists.R")

# clean up
file.remove(outfile)

# close out ----------------------------

print(glue("Team member list written to '{outfile}' at {synid_file_output}."))

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
