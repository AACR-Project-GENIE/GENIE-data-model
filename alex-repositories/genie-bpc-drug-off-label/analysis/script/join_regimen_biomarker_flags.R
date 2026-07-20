library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_hdrug_cohort_lim <- readr::read_rds(
  here('data', 'cohort', 'hdrug_with_conmeds.rds')
)

dft_skel <- dft_hdrug_cohort_lim %>%
  select(cohort, record_id, ca_seq, regimen_number, drug_number,
         dob_drug_start_int)

dft_biom_breast <- readr::read_rds(
  file = here('data', 'cohort', 'biomarker_flags', 
              'biom_breast_1row_per_record.rds')
)

dft_biom_gene <- readr::read_rds(
  file = here('data', 'cohort', 'biomarker_flags', 
              'biom_gene_1row_per_record.rds')
)

dft_biom_lung <- readr::read_rds(
  file = here('data', 'cohort', 'biomarker_flags', 
              'biom_lung_1row_per_record.rds')
)

dft_skel %<>%
  left_join(
    .,
    dft_biom_breast,
    by = c("cohort", "record_id"),
    relationship = 'many-to-one'
  ) %>%
  left_join(
    .,
    dft_biom_gene,
    # I didn't add cohort here - currently should be OK since record alone is
    #   unique.
    by = c('record_id'),
    relationship = 'many-to-one'
  ) %>%
  left_join(
    .,
    dft_biom_lung,
    by = c('cohort', 'record_id'),
    relationship = 'many-to-one'
  )


# goal coding of outputs for biom_time_to_flag():
# T = definitely had the biomarker at time of agent start.
# F = definitely did not have the biomarker at the time of agent start (tested with a negative result).
# NA = no record of a test before this result, so we really don't know.

dft_skel %<>%
  mutate(
    biom_er = biom_time_to_flag(
      tt_ref = dob_drug_start_int,
      tt_test = dob_biom_test_er,
      tt_pos = dob_biom_pos_er
    ),
    biom_pr = biom_time_to_flag(
      tt_ref = dob_drug_start_int,
      tt_test = dob_biom_test_pr,
      tt_pos = dob_biom_pos_pr
    ),
    biom_hr = biom_time_to_flag(
      tt_ref = dob_drug_start_int,
      tt_test = dob_biom_test_hr,
      tt_pos = dob_biom_pos_hr
    ),
    biom_her2 = biom_time_to_flag(
      tt_ref = dob_drug_start_int,
      tt_test = dob_biom_test_her2,
      tt_pos = dob_biom_pos_her2
    )
  )

dft_skel %<>%
  mutate(
    biom_EGFR_exon19del = biom_time_to_flag(
      tt_ref = dob_drug_start_int,
      tt_test = dob_biom_test_EGFR,
      tt_pos = dob_biom_pos_EGFR_exon19del
    ),
    biom_EGFR_pL858R = biom_time_to_flag(
      tt_ref = dob_drug_start_int,
      tt_test = dob_biom_test_EGFR,
      tt_pos = dob_biom_pos_EGFR_pL858R
    ),
    biom_KRAS = biom_time_to_flag(
      tt_ref = dob_drug_start_int,
      tt_test = dob_biom_test_KRAS,
      tt_pos = dob_biom_pos_KRAS
    ),
    biom_BRAF_pV600E = biom_time_to_flag(
      tt_ref = dob_drug_start_int,
      tt_test = dob_biom_test_BRAF,
      tt_pos = dob_biom_pos_BRAF_pV600E
    ),
    biom_EGFR_pT790M = biom_time_to_flag(
      tt_ref = dob_drug_start_int,
      tt_test = dob_biom_test_EGFR,
      tt_pos = dob_biom_pos_EGFR_pT790M
    )
  )

dft_skel %<>%
  mutate(
    biom_pdl1_gte_1 = biom_time_to_flag(
      tt_ref = dob_drug_start_int,
      tt_test = dob_biom_test_pdl1_tps_gte_1,
      tt_pos = dob_biom_pos_pdl1_tps_gte_1
    ),
    biom_pdl1_gte_50 = biom_time_to_flag(
      tt_ref = dob_drug_start_int,
      tt_test = dob_biom_test_pdl1_tps_gte_50,
      tt_pos = dob_biom_pos_pdl1_tps_gte_50
    )
  )
  
# no ALK or EGFR is an odd one, we'll just do that custom:  
dft_skel %<>%
  mutate(
    .tested_for_both = pmax(dob_biom_test_EGFR, dob_biom_test_ALK),
    .first_pos_for_either = pmin(dob_biom_pos_EGFR, dob_biom_pos_ALK, na.rm = T),
    # Same cases as biom_time_to_flag(), just different consequences.
    biom_no_ALK_or_EGFR = case_when(
      is.na(dob_drug_start_int) | is.na(.tested_for_both) ~ NA_real_, 
      dob_drug_start_int > .tested_for_both ~ NA_real_, # not tested at ref time
      is.na(.first_pos_for_either) ~ 1, # never positive => no ALK/EGFR
      dob_drug_start_int >= .first_pos_for_either ~ 0, # pos before drug start.
      dob_drug_start_int < .first_pos_for_either ~ 1, # neg at drug start 
      T ~ -Inf # should never happen, to check.
    )
  ) %>%
  select(-matches("^\\."))

dft_skel %<>%
  mutate(
    across(
      matches("^biom_"),
      .fns = as.logical
    )
  )

# Create the combination biomarkers.
dft_skel %<>%
  mutate(
    biom_hr_and_her2 = biom_combine_and(biom_hr, biom_her2),
    biom_hr_and_her2_neg = biom_combine_and(biom_hr, !biom_her2),
    # tnbc = all of er, pr, her2 must be negative.  hr is neg when er and pr
    #   are both negative, so we can save time with (hr neg) AND (her2 neg).
    biom_tnbc = biom_combine_and(!biom_hr, !biom_her2),
    biom_er_or_her2_neg = biom_combine_or(biom_er, !biom_her2),
    biom_EGFR_ex19_or_pL858R = biom_combine_or(biom_EGFR_exon19del,
                                               biom_EGFR_pL858R),
    biom_pdl1_1_no_ALK_or_EGFR = biom_combine_and(
      biom_pdl1_gte_1,
      biom_no_ALK_or_EGFR
    ),
    biom_pdl1_50_no_ALK_or_EGFR = biom_combine_and(
      biom_pdl1_gte_50,
      biom_no_ALK_or_EGFR
    )
  ) %>%
  # preference:  I like the breast cancer markers to be grouped.
  relocate(
    biom_hr_and_her2, biom_hr_and_her2_neg, biom_tnbc, biom_er_or_her2_neg,
    .after = biom_her2
  )

dft_biom_flags <- dft_skel %>%
  select(cohort, record_id, ca_seq, regimen_number, drug_number,
         matches("^biom_"))

readr::write_rds(
  dft_biom_flags,
  here('data', 'cohort', 'biomarker_flags', "biomarker_flags_by_drug.rds")
)

