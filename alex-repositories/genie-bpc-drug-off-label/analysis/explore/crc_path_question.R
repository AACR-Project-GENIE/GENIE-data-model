library(tidyverse)
library(here)
library(magrittr)

path <- readr::read_csv(
  here('data-raw', 'CRC', 'pathology_report_level_dataset.csv')
)

cpt <- readr::read_csv(
  here('data-raw', 'CRC', 'cancer_panel_test_level_dataset.csv')
)

ca_ind <- readr::read_csv(
  here('data-raw', 'CRC', 'cancer_level_dataset_index.csv')
)

# Find the pathology reports that only have one sample associated.
path_one_samp <- path %>% 
  filter(path_num_spec %in% 1) 

# Quoting from the data guide:
#   The Cancer Panel Test dataset can be linked to ...
#   PRISSMM Pathology dataset using [cohort], [record_id], 
#   [path_proc_number] and [path_report_number]

shared_key <- c('cohort', 'record_id', 'path_proc_number', 'path_rep_number')

path_one_samp %<>%
  # limit to columns user specified an interest in:
  select(all_of(shared_key),
         path_site1, # should be filled for all solo sample reports
         matches("^msi_.*"), matches("^mmr_.*"))

linked_data <- inner_join(
  select(cpt, all_of(shared_key), cpt_genie_sample_id),
  path_one_samp,
  by = shared_key
)

# At this point the cpt_genie_sample_id could be used to link with MAF/CNA/SV
#.  data if further analysis on the genomics is of interest.