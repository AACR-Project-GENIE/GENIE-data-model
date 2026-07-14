# Description: test adding supplemental protein variations to cBioPortal data_mutations_extended.txt
#   with only the columns that are required ("Hugo_Symbol", "NCBI_Build", "HGVSp_Short")
# Author: Haley Hunter-Zinck
# Date: 2022-05-12

# setup ------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)

synid_file_mut <- "syn30157768"
synid_file_ng_pro <- "syn30384837"

# functions ---------------------

#' Get the name of a Synapse entity. 
#' 
#' @param synapse_id Synapse ID string
#' @return String representing entity name
#' @example get_synapse_entity_name("syn12345")
get_synapse_entity_name <- function(synapse_id) {
  return(synGet(synapse_id, downloadFile = F)$properties$name)
}

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
                                           comment.char = "#",
                                           colClasses = "character") {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = check_names,
                   header = header, comment.char = comment.char, colClasses = colClasses)
  return(data)
}

write_df <- function(df_file, filename, delim = "\t", na = "NA") {
  write.table(df_file, row.names = F, sep = delim, file = filename, na = na, quote = F)
  return(filename)
}

# synapse login ----------------

synLogin()

# read --------------

df_pro <- get_synapse_entity_data_in_csv(synid_file_ng_pro)
df_mut <- get_synapse_entity_data_in_csv(synid_file_mut, sep = "\t")

# main -----------------------

# build supplemental rows with protein alterations
df_supp <- data.frame(matrix(NA, nrow = nrow(df_pro), ncol = ncol(df_mut), dimnames = list(c(), colnames(df_mut))))
df_supp["Hugo_Symbol"] <- "ERBB2"
df_supp["Tumor_Sample_Barcode"] <- df_pro["sample_id"]
df_supp["NCBI_Build"] <- df_mut[1,"NCBI_Build"]
df_supp["HGVSp_Short"] <- df_pro["ERBB2 protein change"]

# append supplemental rows
df_final <- df_mut %>%
  bind_rows(df_supp) 

# write -------------------

write_df(df_file = df_final, 
         filename = get_synapse_entity_name(as.character(synid_file_mut)), na = "")

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
