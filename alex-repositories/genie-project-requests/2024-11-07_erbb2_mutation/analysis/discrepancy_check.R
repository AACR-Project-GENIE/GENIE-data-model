library(here)
library(readr)
library(tidyverse)
library(magrittr)

vec_jumi_samp <- readLines(
    here('data', 'jumi_altered_samples.txt')
)

vec_jumi_samp <- vec_jumi_samp %>%
    str_replace_all(., "^genie_private:", "") %>%
    str_trim

clin <- readr::read_rds(
    here('data', 'clin.rds')
) %>%
    rename_all(tolower)

vec_jumi_record <- clin %>%
    filter(sample_id %in% vec_jumi_samp) %>%
    pull(patient_id) %>%
    unique

# run the process.R script to get this object into memory.
# yes, I know that's not great.
alex_record <- nsclc_erbb2 %>% filter(act_list) %>% pull(patient_id) %>% unique

setdiff(alex_record, vec_jumi_record)
discrep <- setdiff(vec_jumi_record, alex_record)

clin %>%
    filter(patient_id %in% discrep) %>%
    glimpse# they don't exist.

mut %>% 
    filter(tumor_sample_barcode %in% discrep)
