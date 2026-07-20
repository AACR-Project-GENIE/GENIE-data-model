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
    # white = case_when(
    #   is.na(naaccr_race_code_primary) ~ NA_real_,
    #   naaccr_race_code_primary %in% "White" ~ 1,
    #   T ~ 0
    # ),
    # Update: Nov 1 2023, recoded this to have white + non-hispanic be the reference.
    # s = simplified.
    race_s = case_when(
      is.na(naaccr_race_code_primary) ~ "race_unk_oth",
      naaccr_race_code_primary %in% "Black" ~ "black",
      naaccr_race_code_primary %in% c("Chinese", "Other Asian") ~ "asian",
      naaccr_race_code_primary %in% c("White") ~ "white",
      T ~ "race_unk_oth" # includes 'Other', 'Unknown' and native north american peoples at this time.
    ),
    hispanic = case_when(
      is.na(naaccr_ethnicity_code) ~ NA_real_,
      naaccr_ethnicity_code %in% "Non-Spanish; non-Hispanic" ~ 0,
      T ~ 1
    )
  ) %>%
  # Also new: Adjustment for site.  We will hide this as an effect.
  select(record_id, institution, race_s, hispanic, birth_year)

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

dft_clin_char %<>% 
  mutate(
    dx_to_dmets_yrs = if_else(stage_dx_iv %in% "Stage IV", 0, dx_to_dmets_yrs),
    tt_os_dmet_yrs = case_when(
      is.na(dx_to_dmets_yrs) ~ NA_real_,
      T ~ tt_os_dx_yrs - dx_to_dmets_yrs,
    )
    # pfs is already relative to stage IV or dmet date.
  ) 

dft_clin_char %<>% 
  mutate(
    stage_dx_iv_num = if_else(stage_dx_iv %in% "Stage IV", 1, 0),
    age_dx_c = age_dx - 40, # approximately centered.
    birth_year_c = birth_year - 1970,
  )

# dummy columns for the categorical predictors with >2 categories
dft_clin_char %<>%
  dummy_cols(., select_columns = "race_s") %>%
  rename_at(
    vars(contains("race_s_")),
    .funs = (function(x) str_replace(x, "^race_s_", ""))
  ) %>%
  select(-race_s, -white) %>% # reference is white non-hispanic now.
  relocate(hispanic, .after = last_col()) 
dft_clin_char %<>%
  dummy_cols(., select_columns = "institution") %>%
  # don't need to rename because we won't show institution effects.
  select(-institution, -institution_MSK) 

# Note: BCA type is categorical but we'll deal with that later since it will
#   be split into separate dataframes first.


readr::write_rds(
  x = dft_clin_char,
  here(
    'data', 'survival', 'v2', 'prepared_data', 
    'clin_char.rds'
  )
)

readr::write_rds(
  x = dft_cpt_dmet,
  here(
    'data', 'survival', 'v2', 'prepared_data', 
    'cpt_dmet.rds'
  )
)

readr::write_rds(
  x = dft_surv_consort,
  here(
    'data', 'survival', 'v2', 'prepared_data', 
    'surv_consort.rds'
  )
)
