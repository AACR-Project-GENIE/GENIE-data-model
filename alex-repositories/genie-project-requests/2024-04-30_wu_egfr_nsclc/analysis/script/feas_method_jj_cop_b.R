# Based on J&J worksheet.

library(purrr); library(fs); library(here)
purrr::walk(.x = fs::dir_ls('R'), .f = source)

dft_ca_ind <- readr::read_csv(
  here('data-raw', 'cancer_level_dataset_index.csv')
)
dft_reg <- readr::read_csv(
  here('data-raw', 'regimen_cancer_level_dataset.csv')
)
dft_ecog <- readr::read_rds(
  here('data', 'ecog_imputed_not_missing.rds')
)


dft_reg %<>%
  mutate(
    regimen_drugs = case_when(
      str_detect(regimen_drugs, "Investigational") ~ "Investigational Regimen",
      T ~ regimen_drugs
    )
  )




# Just going to do this up front for now.  It's too hard to figure out if
#   a second cancer is the metastasized one and such.
dft_cohort <- dft_ca_ind %>%
  group_by(record_id) %>%
  arrange(ca_seq) %>%
  slice(1) %>%
  ungroup(.)

dft_flow <- flow_record_helper(dft_cohort, "People with NSCLC")

dft_mut <- readr::read_rds(here('data', 'samples_with_mut.rds'))
dft_cohort %<>% filter(record_id %in% dft_mut$record_id)

dft_flow %<>% flow_record_helper(dft_cohort, "EGFR L858R or exon 19 inframe del (ever)", .)




dft_dmet <- get_dmet_time(dft_ca_ind, annotate_type = F) %>%
  mutate(dx_dmet_days = dx_dmet_yrs * 365.25) %>%
  select(-dx_dmet_yrs)
# Note:  At the time of this writing there are people who were stage IV with no 
#  mets in the data.  That's an error.  See them with annotate_type = T above.

dft_cohort <- dft_cohort %>%
  left_join(
    .,
    dft_dmet,
    by = c('record_id', 'ca_seq')
  )

dft_cohort %<>%
  mutate(
    dx_adv_or_met_days = case_when(
      stage_dx %in% c("Stage III", "Stage IV") ~ 0,
      !is.na(dx_dmet_days) ~ dx_dmet_days,
      T ~ NA_real_
    )
  )

dft_cohort %<>%
  filter(!is.na(dx_adv_or_met_days))

dft_flow %<>% flow_record_helper(dft_cohort, "Stage 3/4 dx, or met anytime", .)

# A small QC output for chelsea nichols and team (these cases are all errors): 
# dft_ca_ind %>% 
#   filter(stage_dx %in% "Stage IV", !str_detect(ca_dmets_yn, "Yes") | is.na(ca_dmets_yn)) %>% 
#   select(phase, cohort, record_id, ca_seq, stage_dx, ca_dmets_yn) %>% 
#   arrange(ca_dmets_yn) %>%
#   readr::write_csv(., file = here('data', 'nsclc_s4_no_mets.csv'))


# Limit to osi regimens that are on or after adv disease start:
dft_reg_post_adv <- dft_cohort %>%
  select(record_id, ca_seq, dx_adv_or_met_days) %>%
  inner_join(
    .,
    dft_reg,
    by = c('record_id', 'ca_seq'),
    relationship = 'one-to-many'
  )

dft_reg_post_adv %<>%
  # half day tolerance for any rounding:
  filter(dx_reg_start_int >= (dx_adv_or_met_days - 0.5))

# Remove repeats:
dft_reg_post_adv %<>%
  group_by(record_id, ca_seq) %>%
  arrange(regimen_number) %>%
  mutate(
    is_repeat = lag(regimen_drugs) == regimen_drugs
  ) %>%
  ungroup(.) %>%
  filter(!(is_repeat %in% TRUE))

dft_reg_post_adv %<>%
  group_by(record_id, ca_seq) %>%
  mutate(lot = 1:n()) %>%
  ungroup()

dft_cohort <- dft_reg_post_adv %>%
  filter(str_detect(regimen_drugs, "Osimertinib")) %>%
  # Grab only the first Osi use if there are multiple:
  group_by(record_id, ca_seq) %>%
  arrange(regimen_number) %>%
  slice(1) %>%
  ungroup(.) %>%
  mutate(osi_in_adv_setting = T) %>%
  select(
    record_id, ca_seq,
    osi_in_adv_setting,
    osi_regimen_number = regimen_number,
    osi_reg_drugs = regimen_drugs,
    dx_osi_start_days = dx_reg_start_int,
    osi_lot = lot
  ) %>%
  left_join(
    dft_cohort,
    .,
    by = c("record_id", "ca_seq"),
    relationship = 'one-to-one'
  ) %>%
  filter(osi_in_adv_setting)



dft_flow %<>% flow_record_helper(dft_cohort, "Osi in adv/met setting", .)




# Limit to only osi used in the first line setting:
dft_cohort %<>% 
  filter(osi_lot %in% 1)

dft_flow %<>% flow_record_helper(dft_cohort, "Osi 1L (adv/met.)", .)




dft_cohort <- dft_reg_post_adv %>%
  filter(
    str_detect(regimen_drugs, "Pemetrex"),
    str_detect(regimen_drugs, "Carbo")
  ) %>%
  filter(lot %in% 2) %>%
  mutate(carbo_pem_2l = T) %>%
  select(
    record_id, ca_seq,
    carbo_pem_2l,
    cp_regimen_number = regimen_number,
    cp_reg_drugs = regimen_drugs,
    dx_cp_start_days = dx_reg_start_int,
    cp_lot = lot
  ) %>%
  left_join(
    dft_cohort,
    .,
    by = c("record_id", "ca_seq"),
    relationship = 'one-to-one'
  ) 

dft_cohort %<>%
  filter(carbo_pem_2l)

dft_flow %<>% flow_record_helper(dft_cohort, "Carbo/Pem 2L (adv/met.)", .)
  



# Here we use our limitation of one case per person to ignore ca_seq.
dft_ecog_cohort <- dft_cohort %>%
  select(record_id, dob_ca_dx_days, dx_cp_start_days) %>%
  left_join(
    .,
    dft_ecog,
    by = 'record_id'
  ) %>%
  mutate(
    dx_md_onc_visit_days = md_onc_visit_int - dob_ca_dx_days
  )

# Get the baseline ECOG relative to Osi
dft_ecog_baseline <- dft_ecog_cohort %>%
  group_by(record_id) %>%
  filter(dx_md_onc_visit_days > dx_cp_start_days) %>%
  arrange(dx_md_onc_visit_days) %>%
  summarize(
    osi_last_ecog = last(md_ecog_imputed),
    dx_last_md_before_osi = last(dx_md_onc_visit_days),
    .groups = 'drop'
  )

dft_cohort <- left_join(
  dft_cohort,
  dft_ecog_baseline,
  by = 'record_id'
)

dft_cohort %<>% 
  filter(
    str_detect(osi_last_ecog, "0: Fully") | str_detect(osi_last_ecog, "1: ")
  )


dft_flow %<>% flow_record_helper(dft_cohort, "Baseline ECOG of 0 or 1", .)





readr::write_rds(
  dft_flow,
  here('data', 'table_method_jj_cop_b.rds')
)

readr::write_rds(
  dft_cohort,
  here('data', 'final_dat_jj_cop_b.rds')
  
)





