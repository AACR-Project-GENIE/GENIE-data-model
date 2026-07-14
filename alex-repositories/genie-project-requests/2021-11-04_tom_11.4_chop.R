# Description: Check CHOP main GENIE files.  
# Author: Haley Hunter-Zinck
# Date: 2021-11-04 

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_chop <- "syn20446601"

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
#' @return data frame
get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA,
                                           sep = ",", 
                                           na.strings = c("NA"), 
                                           header = T,
                                           check_names = F) {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = check_names,
                   header = header)
  return(data)
}

# read ----------------------------

data8 <- get_synapse_entity_data_in_csv(synid_file_chop, version = 8, sep = "\t")
data7 <- get_synapse_entity_data_in_csv(synid_file_chop, version = 7, sep = "\t")
data6 <- get_synapse_entity_data_in_csv(synid_file_chop, version = 6, sep = "\t")

# main ----------------------------

id8 <- unique(data8$sample_id)
id7 <- unique(data7$sample_id)
id6 <- unique(data6$sample_id)

# plot
grid::grid.draw(venn.diagram(list(id8, id6), category.names = c("Version 8", "Version 6"), filename = NULL))

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
