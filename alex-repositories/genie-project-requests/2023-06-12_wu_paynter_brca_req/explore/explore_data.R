# Description:  See https://sagebionetworks.jira.com/browse/GEN-768
#   Note:  This file is very inefficient computationally.  Lots of data frame
#   lists, lots of room for optimization.
# Author: Alex Paynter

synid_releases <- "syn7492881"
# output_location_synid <- "syn51317177" # in 'GENIE BioPharma Collaborative Internal' > requests

library(synapser)
library(readxl)
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(glue)
library(fs)
library(janitor)
library(here)
library(tictoc)
library(mskcc.oncotree) # the built in oncotree_primary_node was more complete,
# but we will use this if it's not populated.

tic()

purrr::walk(fs::dir_ls(here('R')), .f = source)

synLogin()

# Set up a table for the oncotree codes:
dft_oncotree <- mskcc.oncotree::get_tumor_types() %>%
  select(oncotree_code, tissue) %>%
  # do this to temporarily match the styling of oncotree_primary_node in some
  # of the data.
  mutate(
    tissue = toupper(tissue),
    tissue = str_replace_all(tissue, pattern = " ", replacement = "_")
  )

dft_releases <- get_syn_children_df(synid_releases)

# Recurse to subfolders:
dft_releases %<>%
  rename(major = name) %>%
  mutate(
    child_df = purrr::map(.x = id, .f = get_syn_children_df)
  ) %>%
  select(-c(id, createdOn)) %>%
  unnest(child_df) %>%
  rename(
    minor = name,
    minor_id = id,
    minor_createdOn = createdOn
  )

dft_releases <- dft_releases %>%
  mutate(
    df_file_ids = purrr::map(.x = minor_id, .f = add_select_file_ids)
  ) %>%
  unnest(df_file_ids)

# Excluding some releases because they didn't work with my code, and we
#   don't really need every single one anyway.
excl_releases <- c("8.1-public")
dft_releases %<>%
  filter(!minor %in% excl_releases)


dft_releases %>% 
  filter(minor %in% c("5.2-consortium")) %>%
  mutate(
    # purrr not needed, just easier to use old code
    trash = purrr::pmap(
      .l = list(
        id_mut = data_mutations_extended.txt,
        id_clin_comb = data_clinical.txt,
        id_clin_sample = data_clinical_sample.txt,
        id_clin_pt = data_clinical_patient.txt
      ),
      .f = explore_release_data
    )
  )

dft_releases %>% 
  filter(minor %in% c("8.3-consortium")) %>%
  mutate(
    # purrr not needed, just easier to use old code
    trash = purrr::pmap(
      .l = list(
        id_mut = data_mutations_extended.txt,
        id_clin_comb = data_clinical.txt,
        id_clin_sample = data_clinical_sample.txt,
        id_clin_pt = data_clinical_patient.txt
      ),
      .f = explore_release_data
    )
  )

dft_releases %>% 
  filter(minor %in% c("9.3-consortium")) %>%
  mutate(
    # purrr not needed, just easier to use old code
    trash = purrr::pmap(
      .l = list(
        id_mut = data_mutations_extended.txt,
        id_clin_comb = data_clinical.txt,
        id_clin_sample = data_clinical_sample.txt,
        id_clin_pt = data_clinical_patient.txt
      ),
      .f = explore_release_data
    )
  )

dft_releases %>% 
  filter(minor %in% c("14.1-consortium")) %>%
  mutate(
    # purrr not needed, just easier to use old code
    trash = purrr::pmap(
      .l = list(
        id_mut = data_mutations_extended.txt,
        id_clin_comb = data_clinical.txt,
        id_clin_sample = data_clinical_sample.txt,
        id_clin_pt = data_clinical_patient.txt
      ),
      .f = explore_release_data
    )
  )






toc()

# This would be a reasonable next step, but it's way less efficient to store this way:
# dft_rel_sums %<>% unnest(release_sum)




