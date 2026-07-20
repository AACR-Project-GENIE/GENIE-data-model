library(purrr); library(here); library(fs);
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)


# dft_ca_ind <- readr::read_rds(
#   here('data', 'clin_data_cohort', 'dft_ca_ind.rds')
# )
dft_cpt <- readr::read_rds(
  here('data', 'clin_data_cohort', 'dft_cpt.rds')
)
dft_gene_feat_wide <- readr::read_rds(
  here('data', 'genomic', 'gene_feat_oncogenic.rds')
)
# Clinical characteristics already processed some:
dft_clin_char <- readr::read_rds(
  here(
    'data', 'survival', 'v2', 'prepared_data',
    'clin_char.rds'
  )
)
dft_drug_feas_surv <- readr::read_rds(
  here('data', 'clin_data_cohort', 'drug_feas_surv.rds')
)



dft_drug_surv <- dft_drug_feas_surv %>% 
  filter(crit_all_os) %>%
  select(
    -contains("crit"), 
    -had_met
  )

dft_gene_feat_wide %<>% rename(cpt_genie_sample_id = sample_id)



##################################
### Merge in the clinical data ###
##################################

dft_clin_char_lim <- dft_clin_char %>%
  # Only need some of these:
  select(
    record_id,
    ca_seq,
    bca_subtype_f_simple,
    age_dx, # don't need the ~centered version - we'll recalculate at drug timing.
    white,
    hispanic,
    birth_year_c,
    stage_dx_iv_num
  ) %>%
  mutate(
    bca_subtype_f_simple = forcats::fct_na_value_to_level(
      f = bca_subtype_f_simple,
      level = "(NC or NR)"
    )
  )

# do the dummy coding here (moved up from the surv_fit script as compared with surv_prep_dmet).
dft_clin_char_lim %<>%
  mutate(
    bca = case_when(
      bca_subtype_f_simple %in% "HR+, HER2-" ~ "hr_pos_her2_neg",
      bca_subtype_f_simple %in% "HER2+" ~ "her2_pos",
      bca_subtype_f_simple %in% "Triple Negative" ~ "trip_neg",
      bca_subtype_f_simple %in% "(NC or NR)" ~ "nc_nr",
      T ~ "error"
    )
  ) %>%
  dummy_cols(., select_columns = "bca") %>% 
  # choosing hr_pos_her2_neg as the reference.
  select(-bca, -bca_hr_pos_her2_neg) 

dft_drug_surv <- dft_drug_feas_surv %>% 
  filter(crit_all_os) %>%
  select(-contains("crit"))

dft_drug_surv <- left_join(
  dft_drug_surv,
  dft_clin_char_lim,
  by = "record_id"
) 
dft_drug_surv %<>% relocate(ca_seq, .after = record_id)

dft_drug_surv %<>%
  mutate(
    # this age is not exact.  It's a best guess since age_dx is rounded.
    # + 0.5 because the average person with an integer age of 36 is about 36.5
    #  years old.  Again, this is just approximate, but no reason to be wantonly
    #  incorrect.
    age_drug_start = age_dx + drug_dx_start_int_yrs + 0.5
  ) %>%
  # just to avoid mistakes:
  select(-age_dx)

# Fix the survival variables for this application:
dft_drug_surv %<>%
  mutate(
    tt_os_drug_start_yrs = tt_os_dx_yrs - drug_dx_start_int_yrs,
    # one intermediary variable to get PFS right:
    tt_drug_start_reg_start = drug_dx_start_int_yrs - dx_reg_start_int_yrs,
    tt_pfs_i_and_m_drug_start_yrs = tt_reg_pfs_i_and_m_g_yrs -
      tt_drug_start_reg_start
  ) 

dft_drug_surv %<>%
  rename(pfs_i_and_m_g_status = reg_pfs_i_and_m_g_status) %>%
  # remove anything which could cause confusion:
  select(
    -had_met,
    -tt_drug_start_reg_start,
    -matches('reg'),
    -tt_os_dx_yrs
  ) 

dft_drug_surv %<>%
  relocate(
    os_g_status, 
    .before = tt_os_drug_start_yrs
  ) %>%
  relocate(
    pfs_i_and_m_g_status,
    .before = tt_pfs_i_and_m_drug_start_yrs
  )

# A sanity check here:
chk_os_pfs_times <- dft_drug_surv %>%
  mutate(diff_os_pfs = tt_os_drug_start_yrs - tt_pfs_i_and_m_drug_start_yrs) %>%
  filter(diff_os_pfs < -10^-5) %>%
  select(
    class_comp, 
    record_id, 
    diff_os_pfs, 
    tt_os_drug_start_yrs,
    tt_pfs_i_and_m_drug_start_yrs
  )
if (nrow(chk_os_pfs_times) > 0) {
  print(chk_os_pfs_times)
  cli::cli_abort("Time to OS is less than time to PFS for some rows.")
} 




########################################
### Merge in genomic + NGS test data ###
########################################

# At this point we'll be working with functions that will work better with rows
# defined by unique cases.  As a result, I'll split the dataframe up into 
# a list column.

dft_drug_surv_nest <- dft_drug_surv %>%
  tidyr::nest(.by = class_comp) %>%
  rename(surv = data) %>%
  arrange(class_comp)

dft_drug_surv_nest %<>%
  mutate(
    drug_time = purrr::map(
      .x = surv,
      # for each row, create a dataframe that has just the drug start time
      #  and required key columns.
      .f = (function(x) {
        x %>% select(record_id, ca_seq, drug_dx_start_int_yrs) 
      })
    )
  )

dft_drug_surv_nest %<>%
  mutate(
    cpt_before_drug = purrr::map(
      .x = drug_time,
      .f = (function(x) {
        dat <- get_cpt_by_time(
          time_dat = x,
          time_var = "drug_dx_start_int_yrs",
          cpt_dat = dft_cpt,
          time_tol = 0.001,
          always_keep_first = F
        )
        return(dat)
      })
    )
  )

# Filter the CPT tests down the most recent result.
# If there are two on the same day we prioritize the non-primary (metastatic) 
#.  sample.  If there are two on the same day of the same type we take
#   the highest sequence number anyway, which is arbitrary.
# Also have a selection at the end to limit to columns we care about.
dft_drug_surv_nest %<>%
  mutate(
    cpt_before_drug_most_recent = purrr::map(
      .x = cpt_before_drug,
      .f = (function(x) {
        x %<>%
          mutate(is_metastatic = str_detect(sample_type, "Metas"))
        
        x %<>%
          group_by(record_id) %>%
          arrange(desc(cpt_number), desc(is_metastatic)) %>%
          slice(1) %>%
          ungroup(.)
          
        x %<>%
          select(
            record_id, 
            ca_seq, 
            cpt_number, 
            cpt_genie_sample_id,
            cpt_seq_assay_id
          )
        
        return(x)
      })
    )
  )


# For each NGS test, bring in the genomic results:
dft_drug_surv_nest %<>%
  mutate(
    gene_feat = purrr::map(
      .x = cpt_before_drug_most_recent,
      .f = (function(x) {
        rtn <-  combine_cpt_gene_feat_wide(
          dat_cpt = x,
          dat_gene_feat_wide = dft_gene_feat_wide,
          keep_only_first = F, # should already be unique - just don't mess with it.
          impute_zero_if_not_in_gene_feat = T
        )
        return(rtn)
      })
    )
  ) 

# Merge the genetic results into the survival dataset:
dft_drug_surv_nest %<>%
  mutate(
    surv_with_gene = purrr::map2(
      .x = surv,
      .y = gene_feat,
      .f = (function(x,y) {
        left_join(
          x, y, by = c("record_id", "ca_seq"), relationship = "one-to-one"
        )
      })
    )
  ) 
# Checking rows/cols can be helpful here:
# select(dft_drug_surv_nest, surv, gene_feat, surv_with_gene)

# At this point everything is merged in, we just need one list col:
dft_drug_surv_nest %<>%
  select(
    class_comp,
    dat_surv = surv_with_gene
  )

# Final step:  Have to filter each survival dataset down to the 
#   genetic features which are altered somewhat frequently:
dft_drug_surv_nest %<>%
  mutate(
    # dat_surv_old = dat_surv,
    dat_surv = purrr::map(
      .x = dat_surv,
      # filter the genetic features down to those which are
      #   altered in at least 2% of the samples in the data:
      .f = (function(x) {
        filter_gene_features(
          dat = x,
          prop_filter = 0.02
        )
      })
    )
  )








# Everything should be complete here:
chk_miss <- dft_drug_surv_nest %>%
  mutate(
    miss_chk = purrr::map(
      .x = dat_surv,
      .f = (function(x) {
        x %>%
          # assay_id is missing in some cases - I'm choosing
          #   to ignore it in my check because it doesn't impact
          #   our ability to fit models.
          select(-cpt_seq_assay_id) %>%
          filter(
            # a bit confusing here:  the .x refers to a column.
            # unlike the .x above, which refers to a dataframe in the list column.
            if_any(.cols = everything(), ~is.na(.x))
          )
      })
    )
  ) %>%
  select(class_comp, miss_chk) %>%
  unnest(., cols = miss_chk) 
if (nrow(chk_miss) > 0) {
  cli::cli_abort("Missing rows exist in dft_drug_surv")
}

readr::write_rds(
  x = dft_drug_surv_nest,
  file = here(
    'data', 'survival', 'drug', 'prepared_data',
    'drug_surv_nest.rds'
  )
)
    




  



