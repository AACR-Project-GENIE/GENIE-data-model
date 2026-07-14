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
library(here)
library(tictoc)
# library(mskcc.oncotree) # the built in oncotree_primary_node was more complete,
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

dft_releases %<>%
  filter(name %in% paste0("Release ", c(15, 16)))

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


# Example of doing one:
test_out <- summarize_release_sweeney_TP53_Y220C(
  id_mut = pull(slice(dft_releases, 2), data_mutations_extended.txt),
  id_clin_comb = pull(slice(dft_releases, 2), data_clinical.txt),
  id_clin_sample = NA,
  id_clin_pt = NA
)

# Doing them all:
dft_rel_sums <- dft_releases %>%
  mutate(
    release_sum = purrr::pmap(
      .l = list(
        id_mut = data_mutations_extended.txt,
        id_clin_comb = data_clinical.txt,
        id_clin_sample = data_clinical_sample.txt,
        id_clin_pt = data_clinical_patient.txt
      ),
      .f = summarize_release_wu_brca
    )
  )

# is the output unique by patient?  I think so but worth checking.
# dft_var_ct %>% count(minor, oncotree_primary_node, patient_id, sort = T)
# yes






# Now we just do the analysis, get them numbers:

dft_var_ct <- dft_rel_sums %>%
  select(minor, release_sum) %>%
  unnest(release_sum)

included_onco <- c(
  "LUNG",
  "BREAST",
  "OVARY",
  "OVARY/FALLOPIAN_TUBE",
  "UTERUS"
)
  
dft_var_ct %>%
  filter(oncotree_primary_node %in% included_onco) %>%
  group_by(minor) %>% # ?
  summarize(
    pt_with_variant = sum(variant)
  )
  

dft_var_ct %>%
  filter(oncotree_primary_node %in% included_onco) %>%
  group_by(minor, oncotree_primary_node) %>% # ?
  summarize(
    pt_with_variant = sum(variant, na.rm = T),
    .groups = 'drop'
  ) %>%
  filter(
    str_detect(minor, '15.0|16.1')
  ) %>%
  pivot_wider(
    names_from = oncotree_primary_node,
    values_from = pt_with_variant
  )





