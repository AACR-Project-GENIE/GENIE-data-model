# Based on Jen Hoppe email from May 13.

library(purrr); library(fs); library(here)
purrr::walk(.x = fs::dir_ls('R'), .f = source)


dft_ca_ind <- readr::read_csv(
  here('data-raw', 'cancer_level_dataset_index.csv')
)
dft_reg <- readr::read_csv(
  here('data-raw', 'regimen_cancer_level_dataset.csv')
)

dft_flow <- flow_record_helper(dft_ca_ind, "Raw data")


dft_cohort <- dft_ca_ind

dft_mut <- readr::read_rds(here('data', 'samples_with_mut.rds'))
dft_cohort %<>% filter(record_id %in% dft_mut$record_id)

dft_flow %<>% flow_record_helper(dft_cohort, "EGFR L858R or exon 19 inframe del (ever)", .)


dft_cohort %<>%
  # the "heavy" filter can be used without first limiting to Stage I-III.
  # the "light" filter cannot.
  filter_stage_heavy(.) # separate function
dft_flow %<>% flow_record_helper(dft_cohort, "Confirmed 1B-3A at dx", .)


dft_osi <- dft_reg %>% 
  filter(str_detect(regimen_drugs, "Osimertinib")) %>%
  select(
    record_id, ca_seq, regimen_number,
    regimen_drugs, 
    dob_reg_start_days = drugs_startdt_int_1,
    dx_reg_start_int
  )

dft_cohort <- dft_osi %>% 
  group_by(record_id, ca_seq) %>%
  summarize(osi_ever = T, .groups = "drop") %>%
  left_join(dft_cohort, ., by = c("record_id", "ca_seq")) %>%
  filter(osi_ever)

# Not stated in the steps they wanted to see:
# dft_flow %<>% flow_record_helper(dft_cohort, "Cases with Osimertinib ever", .)



# This is record/ca_seq keyed even though it derives from the path dataset.
dft_exc <- readr::read_rds(here('data', 'first_excision_timing.rds'))

dft_osi_adj <- dft_exc %>%
  select(record_id, ca_seq, dx_fexc_days) %>%
  left_join(., dft_osi, by = c("record_id", "ca_seq")) %>%
  filter(!is.na(regimen_drugs))
dft_osi_adj %<>%
  mutate(drug_adjuvant = dx_reg_start_int - dx_fexc_days > -0.5) %>%
  group_by(record_id, ca_seq) %>% # summarizing over drug uses now.
  summarize(
    any_drug_adjuvant = any(drug_adjuvant, na.rm = T),
    first_adj_regimen_number = first(regimen_number[drug_adjuvant]),
    .groups = "drop"
  )
# Now merge that into the cohort data.
dft_cohort %<>%
  left_join(., dft_osi_adj, by = c("record_id", "ca_seq")) %>%
  filter(any_drug_adjuvant)
dft_flow %<>% flow_record_helper(dft_cohort, "Osi in adjuvant (confirmed with excision record)", .)




# Join the appropriate regimen data in from dft_osi to get progression data
dft_cohort <- left_join(
  dft_cohort,
  dft_osi,
  by = c("record_id", "ca_seq",
         first_adj_regimen_number = "regimen_number"),
  relationship = "one-to-one"
)

# Read the set of computed progression events:
dft_prog <- readr::read_rds(
  here('data', 'prog_events.rds')
)

dft_prog_times <- dft_cohort %>% 
  select(record_id, ca_seq, first_adj_regimen_number, dob_reg_start_days) %>%
  left_join(., dft_prog, by = "record_id") %>%
  # only consider progressions AFTER (not on) the regimen in question.
  filter((dob_prog_days - dob_reg_start_days) > 0.5)  

# Here I'd expect to see no observations less than or equal to zero, and no big identical clusters.
# ggplot(dft_prog_times, aes(x = dob_prog_days - dob_reg_start_days, y = 1)) + geom_jitter()

dft_prog_times %<>%
  mutate(
    cumsum_i = sum(prog_i),
    cumsum_m = sum(prog_m)
  ) %>%
  group_by(record_id, ca_seq, first_adj_regimen_number) %>%
  summarize(
    prog_i_or_m_status = sum(prog_i) > 0 | sum(prog_m) > 0,
    prog_i_and_m_status = sum(prog_i) > 0 & sum(prog_m) > 0,
    # This part is not needed for the data request but it's right here so why not?
    dob_prog_i_or_m_days = first(dob_prog_days[cumsum_i > 0 | cumsum_m > 0]),
    dob_prog_i_and_m_days = first(dob_prog_days[cumsum_i > 0 & cumsum_m > 0]),
    .groups = "drop"
  )

dft_cohort <- left_join(
  dft_cohort,
  dft_prog_times,
  by = c('record_id', 'ca_seq', 'first_adj_regimen_number')
)

dft_cohort %<>% 
  replace_na(
    ., 
    replace = list(
      prog_i_or_m_status = F,
      prog_i_and_m_status = F
    )
  )

dft_cohort %<>% filter(prog_i_or_m_status)
dft_flow %<>% flow_record_helper(dft_cohort, "Progressed (I or M) after adjuvant osi", .) 



dft_cohort <- dft_cohort %>% 
  select(record_id, ca_seq, dob_prog_i_or_m_days) %>%
  left_join(
    .,
    dft_reg,
    by = c("record_id", "ca_seq")
  ) %>%
  group_by(record_id, ca_seq) %>%
  summarize(
    any_after_prog = any(drugs_startdt_int_1 >= dob_prog_i_or_m_days),
    drugs_used_after = first(regimen_drugs[drugs_startdt_int_1 >= dob_prog_i_or_m_days]),
    .groups = "drop"
  )

dft_cohort %<>% filter(any_after_prog)
dft_flow %<>% flow_record_helper(dft_cohort, "1+ regimens after I-or-M progression", .) 




readr::write_rds(
  dft_flow,
  here('data', 'table_method_strict_A.rds')
)







  
    







