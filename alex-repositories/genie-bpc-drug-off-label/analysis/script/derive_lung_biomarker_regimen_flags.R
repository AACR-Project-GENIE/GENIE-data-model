library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

# dft_clin_dat_wide <- readr::read_rds(
#   here('data', 'cohort', 'clin_dat_wide.rds')
# )  

dft_path_lung <- readr::read_csv(
  here('data-raw', 'NSCLC', 'pathology_report_level_dataset.csv')
)

dft_biom_lung <- dft_path_lung %>% 
  filter(pdl1_testing %in% "Yes") %>%
  filter(
    # most confusing dataset structure goes to...
    pdl1_type___1 %in% "Percentage or Percentage Range of Tumor Cells" |
      pdl1_type___2 %in% "Percentage or Percentage Range of Tumor Cells" |
      pdl1_type___3 %in% "Percentage or Percentage Range of Tumor Cells" 
  ) %>%
  # something weird happened with the naming here, just making it consistent:
  rename(
    pdl1_perc_1 = pdl1_perc,
    pdl1_tclrange_1 = pdl1_tclrange
  ) %>%
  select(
    cohort, record_id, path_proc_number, path_rep_number,
    dob_path_proc_days = path_proc_int,
    matches("pdl1_perc_[0-9]"),
    matches("pdl1_tclrange_[0-9]")
  ) 

# The lower ranges are characters.  To simplify I'll fix the NAs, and set
#   <1 to be 0.5.
dft_biom_lung %<>%
  mutate(
    across(
      .cols = matches("pdl1_tclrange_[0-9]"),
      .fns = \(z) {
        case_when(
          z %in% "Not Applicable" ~ NA_real_,
          z %in% "< 1" ~ 0.5, # yep, there's really a space.
          T ~ as.numeric(z)
        )
      }
    )
  )

# There are only a few rows where both a percentage and lower range is reported.
# Our goal is to give people a pass if there is ambiguity, so we will take the
#   maximum of these two as our PD-L1 score.
dft_biom_lung %<>%
  mutate(
    pdl1TPScomb_1 = pmax(pdl1_perc_1, pdl1_tclrange_1, na.rm = T),
    pdl1TPScomb_2 = pmax(pdl1_perc_2, pdl1_tclrange_2, na.rm = T),
    pdl1TPScomb_3 = pmax(pdl1_perc_3, pdl1_tclrange_3, na.rm = T)
  ) %>%
  select(-matches("pdl1_perc_[0-9]"), -matches("pdl1_tclrange_[0-9]"))

dft_biom_lung %<>%
  pivot_longer(
    cols = matches('pdl1TPScomb_')
  ) %>% 
  separate_wider_delim(
    cols = "name",
    delim = "_",
    # I'm using the vague "test_num" here because that's all the info I have.
    # "Up to 3 tests may be associated with each report" - I don't know whether
    #   that means sampling spots or tissue cores or what, but I'll take any
    #   positive as a sign of positivity.
    names = c("test", "test_num"),
    too_few = "error",
    too_many = "error"
  ) %>%
  mutate(test_num = as.numeric(test_num))

# add duplicated rows to cover the ranges we have in our indications sheet:
dft_biom_lung <- bind_rows(
  (dft_biom_lung %>% mutate(test = 'pdl1_tps_gte_1', .thresh = 1)),
  (dft_biom_lung %>% mutate(test = 'pdl1_tps_gte_50', .thresh = 50))
)


dft_biom_lung %<>%
  group_by(cohort, record_id, path_proc_number, path_rep_number, test,
           # just grouping by these to keep them in the data
           dob_path_proc_days) %>%
  summarize(
    biom_tested = any(!is.na(value)),
    biom_pos = any(value >= .thresh, na.rm = T),
    .groups = "drop"
  )

dft_biom_lung %<>% fix_cohort_names(.)

readr::write_rds(
  dft_biom_lung,
  file = here('data', 'cohort', 'biomarker_flags', 'biom_lung_1row_per_test.rds')
)


dft_biom_lung %<>%
  group_by(cohort, record_id, test) %>%
  summarize(
    # The warnings here are full expected because some people are never tested or never positive - no value can be obtained for them.
    dob_biom_tested = suppressWarnings(
      min(dob_path_proc_days[biom_tested], na.rm = T)
    ),
    dob_biom_pos = suppressWarnings(
      min(dob_path_proc_days[biom_pos], na.rm = T)
    ),
    .groups = "drop"
  )

# NA is more intuitive than +Inf for me.
dft_biom_lung %<>%
  mutate(across(.cols = matches("^dob_biom"), .fns = \(z) {
    case_when(is.infinite(z) ~ NA_real_, T ~ z)
  }))
  
# Looking for:  Nothing above horizontal line.
# ggplot(
#   dft_biom_lung,
#   aes(x = dob_biom_tested, y = dob_biom_pos - dob_biom_tested)
# ) +
#   geom_point() +
#   geom_hline(yintercept = 0)


dft_biom_lung %<>%
  rename(dob_biom_test = dob_biom_tested) %>% # slight change
  pivot_longer(
    cols = c(dob_biom_test, dob_biom_pos),
    names_to = "prefix"
  ) %>%
  mutate(col = paste(prefix, test, sep = "_")) %>%
  select(-c(prefix, test)) %>%
  pivot_wider(
    names_from = "col",
    values_from = "value"
  )


readr::write_rds(
  dft_biom_lung,
  file = here('data', 'cohort', 'biomarker_flags', 'biom_lung_1row_per_record.rds')
)
           
    
    
    