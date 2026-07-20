# The results regarding off-label trastuzumab use are a bit wild.
# Want to have a look in the whole cohort to see what's going on.

dft_ca_ind_breast <- readr::read_csv(
  here('data-raw', 'BrCa', 'cancer_level_dataset_index.csv')
)

dft_path_breast <- readr::read_csv(
  here('data-raw', 'BrCa', 'pathology_report_level_dataset.csv')
)

# let's just start with her2:
dft_path_her2 <- dft_path_breast %>% 
  rename(
    path_her2ihc_1 = path_herihc_1,
    path_her2ish_1 = path_herish_1
  ) %>%
  select(
    cohort, record_id, path_proc_number, path_rep_number,
    matches("her2ihc_[1-5]"),
    matches("her2ish_[1-5]")
  ) %>%
  pivot_longer(
    cols = c(path_her2ihc_1:path_her2ish_5)
  ) %>%
  separate_wider_delim(
    cols = "name",
    delim = "_",
    # I'm using the vague "test_num" here because that's all the info I have.
    # "Up to 3 tests may be associated with each report" - I don't know whether
    #   that means sampling spots or tissue cores or what, but I'll take any
    #   positive as a sign of positivity.
    names = c("path", "test", "test_num"),
    too_few = "error",
    too_many = "error"
  ) %>%
  mutate(test_num = as.numeric(test_num)) %>%
  select(-path)

dft_path_her2 %<>%
  filter(!is.na(value))

dft_path_her2_any <- dft_path_her2 %>% 
  group_by(record_id) %>%
  summarize(
    her2_tested_ever = any(!(value %in% "Test not done")),
    her2_equiv_no_pos_ever = any(value %in% "Equivocal") & all(!(value %in% "Positive")),
    her2_pos_ever = any(value %in% "Positive"),
    .groups = "drop"
  )

# going to look at first and only cancers first - easier to be sure here.
dft_her2 <- left_join(
  (dft_ca_ind_breast %>% 
     filter(ca_seq %in% 0) %>% 
     select(record_id, ca_bca_her2ihc_val, ca_bca_her2ihc_intp, ca_bca_her_summ, bca_subtype)),
  dft_path_her2_any,
  by = 'record_id'
) %>%
  # if it's NA at this point we found no path records at all, or no positives, etc.
  replace_na(
    list(her2_tested_ever = F, her2_equiv_no_pos_ever = F, her2_pos_ever = F)
  )

tabyl(dft_her2, ca_bca_her_summ, her2_pos_ever) # yikes.
tabyl(dft_her2, ca_bca_her_summ, her2_equiv_no_pos_ever) # also yikes.
tabyl(dft_her2, ca_bca_her_summ, her2_tested_ever)
 
dft_her2 %>%
  filter(ca_bca_her_summ %in% "Positive/elevated/amplified") %>%
  filter(her2_pos_ever %in% F) %>%
  pull(record_id) %>%
  paste0(., '\n') %>%
  cat

vec_interest <- dft_her2 %>%
  filter(ca_bca_her_summ %in% "Positive/elevated/amplified") %>%
  filter(her2_pos_ever %in% F) %>%
  pull(record_id) 


  



# Separate: lung

dft_path_nsclc <- readr::read_csv(
  here('data-raw', 'NSCLC', 'pathology_report_level_dataset.csv')
)

dft_path_nsclc %>% 
  select(matches("^pdl1")) %>%
  map(.x = ., .f = \(z) mean(is.na(z))) # Looks like we have at least some pdl1 data.

# Pretty good data availability for lung:
dft_path_nsclc %>% count(!is.na(pdl1_perc)) # TPS
dft_path_nsclc %>% count(!is.na(pdl1_num)) # CPS (not used apparently)
dft_path_breast %>% count(!is.na(pdl1_sum)) # 34 - not really usable anyway.

# Not much if any data available for breast:
dft_path_breast %>% count(!is.na(pdl1_perc))
dft_path_breast %>% count(!is.na(pdl1_icperc))
dft_path_breast %>% count(!is.na(pdl1_num))
dft_path_breast %>% count(!is.na(pdl1_num_2))
dft_path_breast %>% count(!is.na(pdl1_sum)) # 34 - not really usable anyway.


