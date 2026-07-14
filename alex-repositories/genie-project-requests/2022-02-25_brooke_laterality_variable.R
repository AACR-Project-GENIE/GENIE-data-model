# Description: check naaccr_laterality_cd format in uploads and cbioportal.
# Author: Haley Hunter-Zinck
# Date: 2022-02-25

library(synapser)
synLogin()

# synapse
synid_file_cbio <- "syn27200442"
synid_file_panc <- "syn25541702"
synid_file_nsclc <- "syn25579921"

col_name <- "naaccr_laterality_cd"

panc <- read.csv(synGet(synid_file_panc)$path)
nsclc <- read.csv(synGet(synid_file_panc)$path)
cbio <- read.csv(synGet(synid_file_cbio)$path, sep = "\t", comment.char = "#")

panc %>% 
  select(naaccr_laterality_cd) %>%
  distinct()

nsclc %>% 
  select(naaccr_laterality_cd) %>%
  distinct()

cbio %>%
  select(NAACCR_LATERALITY_CD) %>%
  distinct()
