# Description: determine if user is not a part of any teams in the permission
#   table reference.
# Author: Haley Hunter-Zinck
# Date: 2021-10-12

# setup --------------------------

tic <- as.double(Sys.time())

library(synapser)
synLogin()

# synapse
syn_table_teams <- "syn25879904"
my_user <- "hhz"

# functions ----------------------------

#' Get all the Synapse teams to which a user belongs.
#' @param user_id Synapse user ID number.  
#' @return_ids If TRUE, return Synapse team ID numbers; otherwise return 
#' Synapse team names.
#' @return vector of Synapse team names or ID numbers
get_user_teams <- function(user_id, return_ids = F) {
  
  team_names <- c()
  
  team_entities <- synRestGET(glue("/user/{user_id}/team/id"))
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

#' Get the user Synapse ID number from the user's Synapse user name.
#' 
#' @param user_name Synapse user name
#' @return Synapse user ID number
#' @example get_user_id("my_user_name")
get_user_id <- function(user_name) {
  
  if (is_user(user_name)) {
    return(synGetUserProfile(user_name)$ownerId)
  }
  
  return(NA)
}

get_team_name <- function(team_id) {
  
  team_name <- rep(NA, length(team_id))
  for (i in 1:length(team_ids)) {
    team_meta <- synRestGET(glue("/team/{team_id[i]}"))
    team_name[i] <- team_meta$name
  }
  
  return(team_name)
}

is_team_member <- function(user_name, team_name) {
  user_id <- get_user_id(user_name)
  user_teams <- get_user_teams(user_id) 
  return(is.element(team_name, user_teams))
}

# main ----------------------------

table_cols <- unlist(lapply(as.list(synGetTableColumns(syn_table_teams)), function(x) {return(x$name)}))
team_ids <- unlist(lapply(strsplit(table_cols[-c(1:4)], split = "-"), tail, n = 1))

membership <- rep(NA, length(team_ids))
names(membership) <- get_team_name(team_ids)
for (i in 1:length(team_ids)) {
  membership[i] <- is_team_member(user_name = my_user, team_name = names(membership[i]))
}

print(membership[!membership])

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
