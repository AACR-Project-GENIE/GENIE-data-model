# Description:  Create data for the start of various drug classes.

library(purrr); library(fs); library(here);
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_drug <- read_rds(
  file = here('data', 'clin_data_cohort', 'drug.rds')
)
dft_ca_ind <- read_rds(
  file = here('data', 'clin_data_cohort', 'dft_ca_ind.rds')
)
dft_cpt <- read_rds(
  file = here('data', 'clin_data_cohort', 'dft_cpt.rds')
)
dft_reg <- read_rds(
  file = here('data', 'clin_data_cohort', 'dft_reg.rds')
)




# First step:  get the first use of each drug class by each person.
dft_drug_sub <- dft_drug %>%
  select(
    record_id,
    regimen_number,
    drug_number,
    agent,
    # these are the two that I think will help me the most:
    dx_drug_start_int,
    drug_start_end_or_lastadm_int,
    contains("class")
  )

dft_drug_sub %<>%
  rename(
    drug_dx_start_int = dx_drug_start_int # just driving me crazy.
  ) %>%
  mutate(
    drug_dx_start_int_yrs = drug_dx_start_int / 365.25,
    drug_dx_end_int_yrs = (drug_start_end_or_lastadm_int + drug_dx_start_int) / 365.25
  ) %>%
  select(
    -drug_start_end_or_lastadm_int,
    -drug_dx_start_int
  )

dft_first_use_class <- dft_drug_sub %>%
  filter(!(exclude_from_class %in% 1)) %>%
  select(-exclude_from_class) %>%
  group_by(record_id, class_comp) %>%
  arrange(regimen_number, drug_number) %>%
  slice(1) %>%
  ungroup(.) %>%
  select(class_comp, record_id, everything()) %>%
  filter(!is.na(class_comp))

write_rds(
  x = dft_first_use_class,
  file = here('data', 'clin_data_cohort', 'first_drug_uses_by_record_class.rds')
)




# Next: Link the drug times with other clinically relevant times:
dft_dmet_timing <- get_dmet_timing(ca_ind_df = dft_ca_ind) %>%
  select(
    record_id,
    dx_dmet_int = tt_y
  )
# Example:  Get all NGS which occur prior to metastatic cancer, or any which are
#   the first NGS test.
dft_first_cpt <- dft_cpt %>%
  group_by(record_id) %>%
  slice(1) %>%
  ungroup %>%
  select(record_id, dob_cpt_report_days)

dft_dob_dx <- dft_ca_ind %>%
  select(record_id, dob_dx_int = ca_cadx_int)

dft_first_cpt <- left_join(
  dft_first_cpt,
  dft_dob_dx,
  by = "record_id"
) 

dft_first_cpt %<>%
  mutate(
    dx_first_cpt_int = (dob_cpt_report_days - dob_dx_int)/365.25
  ) %>%
  select(record_id, dx_first_cpt_int)

dft_clin_timing <- full_join(
  dft_dmet_timing,
  dft_first_cpt,
  by = "record_id"
) %>%
  mutate(
    had_met = if_else(is.na(dx_dmet_int), F, T)
  )

dft_event_timing_dx <- dft_ca_ind %>% 
  select(
    record_id,
    ca_seq,
    dob_dx_int = ca_cadx_int,
    tt_os_dx_yrs,
    # doing days here because we'll redo years in just a moment:
    tt_pfs_i_and_m_adv_days
  ) %>%
  # because they only calculate PFS from the dmet timing and we want everyting
  #  wrt diagnosis, we have to do the work ourselves here:
  left_join(
    .,
    dft_dmet_timing,
    by = c("record_id"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    had_met = if_else(is.na(dx_dmet_int), F, T),
  ) %>%
  select(
    record_id, 
    tt_os_dx_yrs
    # tt_pfs_i_and_m_dx_yrs
  )

dft_clin_timing <- 
  full_join(
    dft_clin_timing,
    dft_event_timing_dx,
    by = "record_id"
  )

dft_drug_feas <- left_join(
  dft_first_use_class,
  dft_clin_timing,
  by = "record_id",
  relationship = "many-to-one"
)



# As a start to PFS feasibility, I want to add in the regimen start time
#   and progression indicators.
# PFS may need to be recalculated depending on the timing of drugs, regimens,
#   and progressions.
dft_drug_feas <- dft_reg %>%
  select(
    record_id, regimen_number, dx_reg_start_int_yrs,
    # adding regimen to these because it's ambiguous to me:
    reg_pfs_i_and_m_g_status = pfs_i_and_m_g_status,
    tt_reg_pfs_i_and_m_g_yrs = tt_pfs_i_and_m_g_yrs,
    os_g_status, # forgot this - adding back in now.
    pfs_i_and_m_g_status
  ) %>%
  mutate(
    tt_reg_pfs_i_and_m_dx_yrs = dx_reg_start_int_yrs + tt_reg_pfs_i_and_m_g_yrs
  ) %>%
  left_join(
    dft_drug_feas,
    ., 
    by = c('record_id', 'regimen_number'),
    relationship = "many-to-one"
  ) 


# Add in the criteria, "crit_", for OS and PFS respectively.
# "_lt_" stands for "less than" below.
dft_drug_feas %<>%
  mutate(
    crit_met_lt_drug = if_else(
      dx_dmet_int <= drug_dx_start_int_yrs,
      T, F, F # just handles the case of NAs.
    ),
    crit_ngs_lt_drug = if_else(
      dx_first_cpt_int <= drug_dx_start_int_yrs,
      T, F, F
    ),
    crit_os_ngs_lt_event = if_else(
      dx_first_cpt_int < tt_os_dx_yrs,
      T, F, F
    ),
    # PFS still needs analysis/work, leaving out for now:
    # crit_pfs_ngs_lt_event = if_else(
    #   dx_first_cpt_int < tt_pfs_i_and_m_dx_yrs,
    #   T, F, F
    # ),
    crit_os_drug_lt_event = if_else(
      drug_dx_start_int_yrs <= tt_os_dx_yrs,
      T, F, F
    ),
    # crit_pfs_drug_lt_event = if_else(
    #   drug_dx_start_int_yrs <= tt_pfs_i_and_m_dx_yrs,
    #   T, F, F
    # )
  )

dft_drug_feas %<>%
  mutate(
    crit_both = crit_met_lt_drug & crit_ngs_lt_drug,
    crit_all_os = crit_both & crit_os_ngs_lt_event & crit_os_drug_lt_event,
    # crit_all_pfs = crit_both & crit_pfs_ngs_lt_event & crit_pfs_drug_lt_event
  ) %>%
  select(
    -crit_both # was just added to be concise above.
  )


write_rds(
  x = dft_drug_feas,
  file = here('data', 'clin_data_cohort', 
              'drug_feas_surv.rds')
)


dft_drug_feas_dmet_sum <- dft_drug_feas %>%
  filter(had_met) %>% 
  group_by(class_comp) %>%
  summarize(
    n_used_agent = n(),
    n_os_dmet = sum(crit_all_os),
    # PFS has to be reworked, leaving out for now:
    # n_pfs_dmet = sum(crit_all_pfs)
  )

write_rds(
  x = dft_drug_feas_dmet_sum,
  file = here('data', 'clin_data_cohort',
              'drug_feas_surv_dmet_sum.rds')
)
      

# Sanity check:  What do the distributions of these time variables look like?
# We will have duplicates here because there are multiple rows for subjects
#   and lots of the times are not drug dependent (e.g. met, ngs).
dft_drug_feas %>%
  filter(!is.na(dx_dmet_int)) %>%
  select(
    record_id, 
    drug_dx_start_int_yrs,
    drug_dx_end_int_yrs,
    dx_dmet_int,
    dx_first_cpt_int,
    tt_os_dx_yrs,
    tt_reg_pfs_i_and_m_dx_yrs
  ) %>%
  pivot_longer(
    cols = -record_id,
    names_to = "var",
    values_to = "t"
  ) %>%
  ggplot(
    dat = .,
    aes(x = t, y = 1)
  ) + 
  geom_jitter(width = 0, height = 0.5) + 
  facet_wrap(vars(var), ncol = 1)
    
  



    
