# Description: Counting genes in bed files for Wake.   
# Author: Haley Hunter-Zinck
# Date: 2022-03-15

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_folder_wake <- "syn12245182"

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

#' Get all child entities of a synapse folder.
#' 
#' @param synapse_id Synapse ID of the folder
#' @param include_types Types of child entities to return
#' @return Vector with values as Synapse IDs and names as entity names.
get_synapse_folder_children <- function(synapse_id, 
                                        include_types=list("folder", "file", "table", "link", "entityview", "dockerrepo")) {
  
  ent <- as.list(synGetChildren(synapse_id, includeTypes = include_types))
  
  children <- c()
  
  if (length(ent) > 0) {
    for (i in 1:length(ent)) {
      children[ent[[i]]$name] <- ent[[i]]$id
    }
  }
  
  return(children)
}


# read ----------------------------

synid_file_children <- get_synapse_folder_children(synid_folder_wake, 
                                              include_types=list("file"))

beds <- list()
for (i in 1:length(synid_file_children)) {
  file_name <- names(synid_file_children)[i]
  if (grepl(pattern = ".bed$", x = file_name)) {
    seq_assay_id <- gsub(pattern = ".bed", replacement = "", x = file_name)
    beds[[seq_assay_id]] <- get_synapse_entity_data_in_csv(as.character(synid_file_children[i]), sep = "\t", header = F)
    colnames(beds[[seq_assay_id]]) <- c("chromosome", "start_position", "end_position", "hugo_symbol","flag")
  }
}

# main ----------------------------

n_gene <- matrix(NA, nrow = length(beds), ncol = 2, dimnames = list(c(), c("seq_assay_id","n_gene")))
for(i in 1:length(beds)) {
  n_gene[i, 1] <- names(beds)[i]
  n_gene[i, 2] = unlist(beds[[i]] %>%
    select(hugo_symbol) %>%
    distinct() %>%
    count())
}

print(n_gene)


# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
