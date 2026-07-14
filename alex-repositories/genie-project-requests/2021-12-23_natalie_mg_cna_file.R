# Description: Check existing main GENIE CNA file submissions to double-check if
#   all genes and samples need to be included.  
# Author: Haley Hunter-Zinck
# Date: 2021-12-23

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_cna <- "syn7224464"
synid_file_seg <- "syn7224473"
synid_file_sam <- "syn7224474"
synid_file_mut <- "syn7224467"

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

cna <- get_synapse_entity_data_in_csv(synid_file_cna, sep = "\t")
n_gene_cna <- nrow(cna)
n_sam_cna <- ncol(cna) - 1
rm(cna)

sam <- get_synapse_entity_data_in_csv(synid_file_sam, sep = "\t")
n_sam_sam <- unlist(sam %>% select(SAMPLE_ID) %>% distinct() %>% count())

mut <- get_synapse_entity_data_in_csv(synid_file_mut, sep = "\t")
n_gene_mut <- unlist(mut %>% select(Hugo_Symbol) %>% distinct() %>% count())

# main ----------------------------

print(glue("Number of recorded genes in mutation file greater than in cna file? {n_gene_mut > n_gene_cna}"))
print(glue("Number of recorded genes in mutation file equal to that in cna file? {n_gene_mut == n_gene_cna}"))

print(glue("Number of recorded samples greater than in cna file? {n_sam_sam > n_sam_cna}"))
print(glue("Number of recorded samples equal to that in cna file? {n_sam_sam == n_sam_cna}"))

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
