# Description: Check the 2.1 NSLC BPC consortium release for BPC retracts sample and patient IDs.
# Author: Haley Hunter-Zinck
# Date: July 21, 2021

library(synapser)
library(glue)
synLogin()

synid_file_pt <- "syn25985884" # BPC, NSCLC, 2.1, patient_level_dataaset.csv
synid_file_sa <- "syn25985889" # BPC, NSCLC, 2.1, cancer_panel_level_level_dataaset.csv
synid_table <- "syn25779833"  # BPC-internal, BPC Sample Retraction table

# read three data sets
data_pt <- read.csv(synGet(synid_file_pt)$path, stringsAsFactors = F, na.strings = "")
data_sa <- read.csv(synGet(synid_file_sa)$path, stringsAsFactors = F, na.strings = "")
data_re <- as.data.frame(synTableQuery(glue("SELECT SAMPLE_ID FROM {synid_table}")))

# get sample and patient IDs
all_pt <- data_pt$record_id
all_sa <- data_sa$cpt_genie_sample_id
re_sa <- data_re$SAMPLE_ID
re_pt <- unique(unlist(lapply(lapply(strsplit(data_re$SAMPLE_ID, split = "-"), head, n = 3), paste0, collapse = "-")))

# examine intersection
inter_pt <- intersect(all_pt, re_pt)
inter_sa <- intersect(all_sa, re_sa)

# print results
print_summary <- function() {
  print(glue("Number of retracted samples: {length(re_sa)}"))
  print(glue("Retracted patient IDs present in NSCLC v2.1 release ({length(inter_pt)} / {length(re_pt)})"))
  print(inter_pt)
  print(glue("Retracted sample IDs present in NSCLC v2.1 release ({length(inter_sa)} / {length(re_sa)})"))
  print(inter_sa)
}
print_summary()

