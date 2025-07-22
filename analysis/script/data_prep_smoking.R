library(here)
library(tidyverse)
library(magrittr)

ca_ind_nsclc <- readr::read_csv(
  here('data-raw', 'NSCLC', 'cancer_level_dataset_index.csv')
)

pt_nsclc <- readr::read_csv(
  here('data-raw', 'NSCLC', 'patient_level_dataset.csv')
)

# grab the target (smoking) and features which could reasonably be considered baseline.
smoke_dat <- ca_ind_nsclc |>
  select(
    record_id,
    ca_seq,
    dob_ca_dx_yrs,
    ca_dx_how,
    ca_type,
    ca_hist_adeno_squamous,
    stage_dx,
    # all the TNM gibberish makes me nervous - leaving for now:
    # naaccr_path_t_cd,
    # ca_path_t_stage,
    # naaccr_path_n_cd,
    # ca_path_n_stage,
    # naaccr_path_m_cd,
    # ca_dmets_yn, # equivalent curated metastatic.
    # naaccr_clin_t_cd,
    # ca_clin_t_stage,
    # naaccr_clin_n_cd,
    # ca_clin_n_stage,
    # naaccr_clin_m_cd,
    ca_lung_sep_tumor,
    ca_lung_pl_el_invasion
  )

# for each patient just take their first cancer.
smoke_dat %<>%
  group_by(record_id) %>%
  arrange(ca_seq) %>%
  slice(1) %>%
  ungroup(.)

smoke_dat <- pt_nsclc %>%
  select(
    record_id,
    institution,
    birth_year,
    naaccr_ethnicity_code,
    naaccr_race_code_primary,
    naaccr_sex_code
  ) %>%
  left_join(
    smoke_dat,
    .,
    by = 'record_id',
    relationship = 'one-to-one'
  )

# need to melt down: sites of distant mets.
