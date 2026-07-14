# Description:  Pull the clinical data and save it.  Simple.
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
library(ggplot2)

purrr::walk(fs::dir_ls(here('R')), .f = source)

synLogin()

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

incl_releases <- c("14.1-public")
dft_releases %<>%
  filter(minor %in% incl_releases)

dft_releases

gene_panels <- get_syn_children_df(dft_releases$minor_id) %>%
  pull(name)



dft_releases <- dft_releases %>%
  mutate(
    df_file_ids = purrr::map(.x = minor_id, .f = add_select_file_ids)
  ) %>%
  unnest(df_file_ids)


test_pt_public <- get_synapse_entity_txt(
  pull(slice(dft_releases, 1), data_clinical_patient.txt), 
  skip = 0
)

test_samp_public <-  get_synapse_entity_txt(
  pull(slice(dft_releases, 1), data_clinical_sample.txt), 
  skip = 0
)

test_both_consortium <- get_synapse_entity_txt(
  pull(slice(dft_releases, 2), data_clinical.txt), 
  skip = 0
)


test_pt_consortium <- get_synapse_entity_txt(
  pull(slice(dft_releases, 2), data_clinical_patient.txt), 
  skip = 0
)

test_samp_consortium <- get_synapse_entity_txt(
  pull(slice(dft_releases, 2), data_clinical_sample.txt), 
  skip = 0
)

#######
# DMZ #
#######

# Difference between the combined and separated consortium files:
setdiff(  
  c(names(test_pt_consortium), names(test_samp_consortium)),
  c(names(test_both_consortium))
)
setdiff(  
  c(names(test_both_consortium)),
  c(names(test_pt_consortium), names(test_samp_consortium))
)
# Consortium combined file has seq_date and oncotree info.. weird.


# Columns in the consortium release but not the public release:
setdiff(  
  c(names(test_pt_consortium), names(test_samp_consortium)),
  c(names(test_pt_public), names(test_samp_public))
)
setdiff(  
  c(names(test_both_consortium)),
  c(names(test_pt_public), names(test_samp_public))
)

# Columns in public release but not the consortium release:
setdiff(
  c(names(test_pt_public), names(test_samp_public)),
  names(test_both_consortium)
)



unique_non_number <- function(vec) {
  vec <- unique(vec)
  ind <- stringr::str_detect(vec, "^[0-9].*$", negate = T)
  vec[ind]
}

test_pt_public %>% 
  select(int_contact, int_dod, year_contact, dead, year_death) %>%
  purrr::map(
    .f = unique_non_number
  )

test_pt_consortium %>% 
  mutate(
    int_dod_num = as.numeric(int_dod),
    int_contact_num = as.numeric(int_contact)
  ) %>%
  filter(
    int_dod_num > int_contact_num
  )

test_pt_consortium %>% glimpse
test_pt_consortium %>% 
  mutate(
    year_death_num = as.numeric(year_death),
    year_contact_num = as.numeric(year_contact)
  ) %>%
  filter(
    year_contact_num >= 89 & year_contact_num <90
  )

test_samp_public %>% 
  select(age_at_seq_report) %>%
  purrr::map(
    .f = unique_non_number
  )


test_clin %>%
  summarize(
    across(
      .cols = everything(),
      .fns = ~ sum(is.na(.))
    )
  ) %>%
  glimpse

miss_agree_mat <- test_clin %>%
  mutate(
    across(
      .cols = everything(),
      .fns = is.na
    )
  ) %>%
  pct_agree_mat

plot_pct_agree(
  mat = miss_agree_mat,
  remove_diag = F
)

