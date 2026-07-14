library(here)
library(tidyverse)
library(magrittr)

ca_ind_nsclc <- readr::read_csv(
  here('data-raw', 'NSCLC', 'cancer_level_dataset_index.csv')
)

cpt_nsclc <- readr::read_csv(
  here('data-raw', 'NSCLC', 'cancer_panel_test_level_dataset.csv')
)

pt_nsclc <- readr::read_csv(
  here('data-raw', 'NSCLC', 'patient_level_dataset.csv')
)

# grab the target (smoking) and features which could reasonably be considered baseline.
smoke_dat <- ca_ind_nsclc |>
  select(
    record_id,
    ca_seq,
    ca_cigarette,
    dob_ca_dx_yrs,
    ca_dx_how,
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

# Put in the CPT data that's relevant from everyone's first sample.
smoke_dat <- cpt_nsclc %>%
  select(
    record_id,
    ca_seq,
    cpt_number,
    cpt_genie_sample_id,
    dob_cpt_report_days,
    path_proc_cpt_rep_days,
    sample_type
  ) %>%
  # grab the first sample in each cancer
  group_by(record_id, ca_seq) %>%
  arrange(cpt_number) %>%
  slice(1) %>%
  ungroup(.) %>%
  # join to the smoking data:
  left_join(
    smoke_dat,
    .,
    by = c('record_id', 'ca_seq'),
    relationship = 'one-to-one'
  )

smoke_dat %<>%
  mutate(
    dx_cpt_rep_days = round(dob_cpt_report_days - (dob_ca_dx_yrs) * 365.25)
  ) %>%
  select(-dob_cpt_report_days)

met_sites_wide <- ca_ind_nsclc |>
  select(record_id, ca_seq, matches("ca_first_dmets")) %>%
  mutate(
    across(
      .cols = matches("ca_first_dmets"),
      .fns = \(x) {
        x <- str_replace_all(x, "[:blank:]", "_")
        x <- str_replace_all(x, "-", "_")
        return(x)
      }
    )
  ) %>%
  pivot_longer(
    cols = matches("ca_first_dmets"),
    names_to = "junk",
    values_to = "met_site"
  )

met_sites_wide <- met_sites_wide %>%
  select(-junk) %>%
  mutate(val = T) %>%
  filter(!is.na(met_site)) %>%
  # just doing the str_sub because otherwise glimpse() is horrible:
  mutate(met_site = str_sub(paste0("base_dmet_", met_site), 1, 35))

met_sites_wide %<>% distinct(.) # couple duplicates.

met_sites_wide %<>%
  pivot_wider(
    names_from = 'met_site',
    values_from = 'val',
    values_fill = F
  )

smoke_dat <-
  left_join(
    smoke_dat,
    met_sites_wide,
    by = c('record_id', 'ca_seq')
  )

smoke_dat <- smoke_dat %>%
  mutate(across(starts_with("base_dmet"), ~ replace_na(., FALSE)))

smoke_dat %<>%
  mutate(
    ca_cig_current = case_when(
      ca_cigarette %in% "Current user" ~ T,
      T ~ F
    ),
    # probably the better feature to test for genomic signatures:
    ca_cig_ever = case_when(
      ca_cigarette %in% "Current user" ~ T,
      str_detect(ca_cigarette, "Former user") ~ T,
      T ~ F
    )
  ) %>%
  relocate(
    ca_cig_current,
    ca_cig_ever,
    .after = ca_cigarette
  )

write_rds(
  smoke_dat,
  here('data', 'smoke', 'smoke_clin.rds')
)
