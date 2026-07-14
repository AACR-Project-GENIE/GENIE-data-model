# Description: test fastest way to query permissions on a Synapse entity.  
# Author: Haley Hunter-Zinck
# Date: 2021-

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(synapser)
synLogin()

# synapse

# functions ----------------------------

#' Check whether a string represents a user name.
#' 
#' @param personage_name user or other Synapse entity name
#' @return TRUE if name relates to a Synapse user; otherwise FALSE
#' @example is_user("my_user_name")
is_user <- function(personage_name) {
  res <- tryCatch(length(synGetUserProfile(personage_name)), 
                  error = function(cond) {return(0)})
  
  return(as.logical(res))
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

get_permissions_synapser <- function(synapse_id, personage_name) {
  return(synGetPermissions(synapse_id, personage_name))
}

get_permissions_rest <- function(synapse_id, personage_id) {
  
  acl <- tryCatch({
      synRestGET(glue("/entity/{synapse_id}/acl"))
    }, error = function(cond) {
      return(synRestGET(tail(strsplit(cond[[1]], split = " ")[[1]], 1)))
    })
  
  personage_ids <- unlist(lapply(acl$resourceAccess, function(x) {return(x$principalId)}))
  
  return(acl$resourceAccess[[which(personage_ids == personage_id)]]$accessType)
}

# main ----------------------------

synapse_id <- "syn26342236"
personage_name = "hhz"
personage_id <- get_user_id(personage_name)

tic_synapser_name <- as.double(Sys.time())
res_synapser_name <- get_permissions_synapser(synapse_id, personage_name)
toc_synapser_name <- as.double(Sys.time())

tic_synapser_id <- as.double(Sys.time())
res_synapser_id <- get_permissions_synapser(synapse_id, personage_id)
toc_synapser_id <- as.double(Sys.time())

tic_rest <- as.double(Sys.time())
res_rest <- get_permissions_rest(synapse_id, personage_id)
toc_rest <- as.double(Sys.time())


# close out ----------------------------

print(glue("Identical results: {identical(sort(unlist(res_synapser_id)), sort(unlist(res_rest)))}"))
print(glue("Runtime (synapser, name): {toc_synapser_name - tic_synapser_name}"))
print(glue("Runtime (synapser, ID): {toc_synapser_id - tic_synapser_id}"))
print(glue("Runtime (REST API): {toc_rest - tic_rest}"))
toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
