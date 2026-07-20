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
synid_ver_maf <- "281"
synid_file_duke <- "syn17115022"
synid_ver_duke <- "9"
synid_file_duke_proc <- "syn17078118"
synid_ver_duke_proc <- "64"

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

# read ----------------------------

maf <- get_synapse_entity_data_in_csv(synid_file_maf, version = synid_ver_maf, sep = "\t")
duke <- get_synapse_entity_data_in_csv(synid_file_duke, version = synid_ver_duke, sep = "\t")
duke_proc <- get_synapse_entity_data_in_csv(synid_file_duke_proc, version = synid_ver_duke_proc, sep = "\t")

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
print(glue("Number of duplicated variants: {sum(ind_dup)}"))
print(dup_by_center)

nondup_by_center <- maf %>%
  filter(!ind_dup) %>%
  group_by(Center) %>%
  count()
print(glue("Number of unique variants: {sum(!ind_dup)}"))
print(nondup_by_center)

# check duke raw upload
ind_dup_duke <- duke %>% 
  select(Chromosome, Start_Position, Reference_Allele, Tumor_Seq_Allele2, Tumor_Sample_Barcode) %>%
  duplicated()
print(glue("Number of duplicated variants in Duke upload: {sum(ind_dup_duke)}"))

# check duke processed upload
ind_dup_duke_proc <- duke_proc %>% 
  select(Chromosome, Start_Position, Reference_Allele, Tumor_Seq_Allele2, Tumor_Sample_Barcode) %>%
  duplicated()
print(glue("Number of duplicated variants in proccessed Duke upload: {sum(ind_dup_duke_proc)}"))

# check number of times each duplicated for one center 
for (center in dup_by_center$Center) {
  
  dup_count_prov <- maf %>%
    filter(Center == center) %>%
    group_by(Chromosome, Start_Position, Reference_Allele, Tumor_Seq_Allele2, Tumor_Sample_Barcode) %>%
    count() 
  
  hist_dup <- table(dup_count_prov$n)
  for (i in 1:length(hist_dup)) {
    print(glue("Number of times each {center} variant is occurs {names(hist_dup)[i]} time(s): {hist_dup[i]}"))
  }
}

tmp <- maf %>% 
  mutate(dup_status <- ind_dup) %>%
  filter(Center == 'PROV') %>%
  arrange(Chromosome, Start_Position, Reference_Allele, Tumor_Seq_Allele2, Tumor_Sample_Barcode)

# look at variant type
maf %>% 
  filter(ind_dup) %>%
  select(Variant_Type) %>%
  distinct()
maf %>% 
  filter(!ind_dup) %>%
  select(Variant_Type) %>%
  distinct()

# chromosome
maf %>% 
  filter(ind_dup) %>%
  select(Chromosome) %>%
  distinct()
maf %>% 
  filter(!ind_dup) %>%
  select(Chromosome) %>%
  distinct()

# SCI duplicated variant
maf %>%
  filter(ind_dup & Center == 'SCI') %>%
  select(Chromosome, Start_Position, Reference_Allele, Tumor_Seq_Allele2, Tumor_Sample_Barcode)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
