# Description: This was originally a part of surv_prep_dmet_2.R.  We moved it
#   here since it's reused several times now.

library(purrr); library(fs); library(here);
purrr::walk(.x = fs::dir_ls('R'), .f = source)

dft_pt <- readr::read_rds(
  here('data', 'clin_data_cohort', 'dft_pt.rds')
)
dft_ca_ind <- readr::read_rds(
  here('data', 'clin_data_cohort', 'dft_ca_ind.rds')
)
dft_cpt <- readr::read_rds(
  here('data', 'clin_data_cohort', 'dft_cpt.rds')
)
dft_reg <- readr::read_rds(
  here('data', 'clin_data_cohort', 'dft_reg.rds')
)

# Doing this now for my cohort tracking - possibly could do it at the start.
dft_ca_ind %<>% 
  mutate(
    bca_subtype_f_simple = forcats::fct_na_value_to_level(
      f = bca_subtype_f_simple,
      level = "(NC or NR)"
    )
  )

dft_surv_consort <- surv_cohort_track_help(
  dft_ca_ind, state = "start"
)



dft_dmet_timing <- get_dmet_timing(ca_ind_df = dft_ca_ind)

# Example:  Get all NGS which occur prior to metastatic cancer, or any which are
#   the first NGS test.
dft_cpt_dmet <- get_cpt_by_time(
  time_dat = dft_dmet_timing,
  time_var = "tt_y",
  cpt_dat = dft_cpt,
  always_keep_first = T
)

dft_cpt_dmet %<>%
  select(
    record_id, 
    ca_seq, 
    cpt_genie_sample_id,
    dx_cpt_rep_yrs,
    is_first_cpt,
    cpt_before_t,
    cpt_seq_assay_id
  )

dft_clin_char <- dft_pt %>%
  mutate(
    white = case_when(
      is.na(naaccr_ethnicity_code) ~ NA_real_,
      naaccr_race_code_primary %in% "White" ~ 1,
      T ~ 0
    ),
    hispanic = case_when(
      is.na(naaccr_ethnicity_code) ~ NA_real_,
      naaccr_ethnicity_code %in% "Non-Spanish; non-Hispanic" ~ 0,
      T ~ 1
    )
  ) %>%
  select(record_id, white, hispanic, birth_year)

dft_clin_char <- dft_ca_ind %>%
  select(
    record_id, 
    ca_seq, # works because we've already selected one row per person.
    contains("bca_subtype"), # several versions.
    age_dx, 
    stage_dx_iv,
    dmets_stage_i_iii,
    dx_to_dmets_yrs,
    os_dx_status, 
    tt_os_dx_yrs,
    pfs_i_and_m_adv_status,
    tt_pfs_i_and_m_adv_yrs
  ) %>%
  full_join(., dft_clin_char, by = "record_id") 

dft_first_cpt <- dft_cpt %>% 
  group_by(record_id, ca_seq) %>%
  slice(1) %>%
  ungroup(.) %>%
  select(
    record_id, ca_seq, 
    dx_fcpt_rep_days = dx_cpt_rep_days, 
    dx_fcpt_rep_yrs = dx_cpt_rep_yrs
  )

dft_clin_char %<>%
  left_join(., dft_first_cpt, by = c('record_id', 'ca_seq'))

readr::write_rds(
  x = dft_cpt_dmet,
  here(
    'data', 'survival', 'v2', 'prepared_data', 
    'cpt_dmet.rds'
  )
)

readr::write_rds(
  x = dft_clin_char,
  here(
    'data', 'survival', 'v2', 'prepared_data', 
    'clin_char_dmet.rds'
  )
)

