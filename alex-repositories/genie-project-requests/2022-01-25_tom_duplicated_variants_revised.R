# Description: Investigate duplicated variants in release 12.1-consortium.
# Author: Haley Hunter-Zinck
# Date: 2022-01-24

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_maf <- "syn5571527"
synid_ver_maf <- "284"
synid_file_uchi <- "syn12978929"
synid_ver_uchi <- "2"
synid_file_uchi_proc <- "syn15673036"
synid_ver_uchi_proc <- "74"
synid_ver_uchi_prev <- "71"
synid_file_uchi_flat <- "syn22275379"
synid_ver_uchi_flat <- "24"

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

maf <- get_synapse_entity_data_in_csv(synid_file_maf, version = synid_ver_maf, sep = "\t")
uchi <- get_synapse_entity_data_in_csv(synid_file_uchi, version = synid_ver_uchi, sep = "\t")
uchi_proc <- get_synapse_entity_data_in_csv(synid_file_uchi_proc, version = synid_ver_uchi_proc, sep = "\t")
uchi_prev <- get_synapse_entity_data_in_csv(synid_file_uchi_proc, version = synid_ver_uchi_prev, sep = "\t")
uchi_flat <- get_synapse_entity_data_in_csv(synid_file_uchi_flat, version = synid_ver_uchi_flat, sep = "\t")

# main ----------------------------

print(glue("Number of rows: {nrow(maf)}"))

# number of duplicates by center
ind_dup <- maf %>% 
  select(Chromosome, Start_Position, Reference_Allele, Tumor_Seq_Allele2, Tumor_Sample_Barcode) %>%
  duplicated()
dup_by_center <- maf %>%
  filter(ind_dup) %>%
  group_by(Center) %>%
  count()
print(glue("Number of duplicated variants in release maf ({synid_file_maf}.{synid_ver_maf}): {sum(ind_dup)}"))
print(dup_by_center)

nondup_by_center <- maf %>%
  filter(!ind_dup) %>%
  group_by(Center) %>%
  count()
print(glue("Number of unique variants: {sum(!ind_dup)}"))
print(nondup_by_center)

# check uchi raw upload
ind_dup_uchi <- uchi %>% 
  select(Chromosome, Start_Position, Reference_Allele, Tumor_Seq_Allele2, Tumor_Sample_Barcode) %>%
  duplicated()
print(glue("Number of duplicated variants in raw uchi upload ({synid_file_uchi}.{synid_ver_uchi}): {sum(ind_dup_uchi)}"))

# check uchi processed upload
ind_dup_uchi_proc <- uchi_proc %>% 
  select(Chromosome, Start_Position, Reference_Allele, Tumor_Seq_Allele2, Tumor_Sample_Barcode) %>%
  duplicated()
print(glue("Number of duplicated variants in proccessed uchi upload ({synid_file_uchi_proc}.{synid_ver_uchi_proc}): {sum(ind_dup_uchi_proc)}"))

# check previous uchi processed upload
ind_dup_uchi_prev <- uchi_prev %>% 
  select(Chromosome, Start_Position, Reference_Allele, Tumor_Seq_Allele2, Tumor_Sample_Barcode) %>%
  duplicated()
print(glue("Number of duplicated variants in previous proccessed uchi upload ({synid_file_uchi_proc}.{synid_ver_uchi_prev}): {sum(ind_dup_uchi_prev)}"))

ind_dup_uchi_flat <- uchi_flat %>% 
  select(Chromosome, Start_Position, Reference_Allele, Tumor_Seq_Allele2, Tumor_Sample_Barcode) %>%
  duplicated()
print(glue("Number of duplicated variants in uchi flat file ({synid_file_uchi_flat}.{synid_ver_uchi_flat}): {sum(ind_dup_uchi_flat)}"))

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
