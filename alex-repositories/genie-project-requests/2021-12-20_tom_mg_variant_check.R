# Description: Check for variant that is marked as an artifact but not shown in cBioPortal.
# Author: Haley Hunter-Zinck
# Date: 2021-12-20

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_mutation <- "syn5571527"
synid_file_artifact <- "syn26561497"
synid_file_nonsomatic <- "syn26533529"
version_11_5 <- 278
version_11_6 <- 279

# mutation
chromosome <- "1"
bp_start <- "6528337"
bp_end <- "6528337"
gene <- "PLEKHG5"

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

mut5 <- get_synapse_entity_data_in_csv(synid_file_mutation, sep = "\t", version = version_11_5)
mut6 <- get_synapse_entity_data_in_csv(synid_file_mutation, sep = "\t", version = version_11_6)
artifact <- get_synapse_entity_data_in_csv(synid_file_artifact, sep = "\t")
nonsomatic <- get_synapse_entity_data_in_csv(synid_file_nonsomatic, sep = ",")

# main ----------------------------

# confirm presence in mutation file
n_mut5 <- unlist(mut5 %>% 
  filter(Hugo_Symbol == gene & Start_Position == bp_start) %>%
  select(Chromosome, Start_Position, End_Position, Reference_Allele,Tumor_Seq_Allele1, Tumor_Seq_Allele2, Tumor_Sample_Barcode) %>%
  count())

n_mut6 <- unlist(mut6 %>% 
  filter(Hugo_Symbol == gene & Start_Position == bp_start) %>%
  select(Chromosome, Start_Position, End_Position, Reference_Allele,Tumor_Seq_Allele1, Tumor_Seq_Allele2, Tumor_Sample_Barcode) %>%
  count())

n_mut6_type <- unlist(mut6 %>% 
  filter(Hugo_Symbol == gene & Start_Position == bp_start) %>%
  select(Consequence) %>%
  distinct())

# confirm presence in artifact file
n_artifact <- unlist(artifact %>%
  filter(Hugo_Symbol == gene) %>%
  count())

# confirm lack of presence in identified non-somatic variants
n_nonsomatic <- unlist(nonsomatic %>%
  filter(Hugo_Symbol == gene) %>%
  count())

# close out ----------------------------

print(glue("Number of matching mutations in 11.5 mutation file: {n_mut5}"))
print(glue("Number of matching mutations in 11.6 mutation file: {n_mut6}"))
print(glue("Type of variant: {n_mut6_type}"))
print(glue("Number of matching mutations in 11.6 artifact file: {n_artifact}"))
print(glue("Number of matching mutations in 11.6 non-somatic file: {n_nonsomatic}"))

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
