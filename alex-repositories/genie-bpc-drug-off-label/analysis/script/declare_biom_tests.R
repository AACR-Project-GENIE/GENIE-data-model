# This script declares function arguments to be run with 
#   add_check_with_simple().
# These are concomitant drug requirements (and monotherapy), noted
#   using the 'with' column in the HemOnc sheet.

library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

simple_biom_tests <- tibble(
  ind_sheet_bio_req = character(0),
  # agent_req can be length >= 2, so we need a list column:
  biom_col = character(0),
  test_name = character(0)
)

# breast cancer related
simple_biom_tests %<>%
  add_row(
    ind_sheet_bio_req = "HER2+",
    biom_col = "biom_her2"
  ) %>% 
  add_row(
    ind_sheet_bio_req = "HR+",
    biom_col = "biom_hr"
  ) %>%
  add_row(
    ind_sheet_bio_req = "HR+ and HER2-",
    biom_col = "biom_hr_and_her2_neg"
  ) %>%
  add_row(
    ind_sheet_bio_req = "HR+ and HER2+",
    biom_col = "biom_hr_and_her2"
  ) %>%
  add_row(
    ind_sheet_bio_req = "ER+|HER2-",
    biom_col = "biom_er_or_her2_neg"
  ) %>%
  add_row(
    ind_sheet_bio_req = "TNBC",
    biom_col = "biom_tnbc"
  ) %>%
  add_row(
    ind_sheet_bio_req = "ER+",
    biom_col = "biom_er"
  ) 

# Lung biomarkers
simple_biom_tests %<>%
  add_row(
    ind_sheet_bio_req = "PD-L1 TPS at least 1%",
    biom_col = "biom_pdl1_gte_1",
  ) %>%
  add_row(
    ind_sheet_bio_req = "PD-L1 TPS at least 1% AND No EGFR mutations AND No ALK mutations",
    biom_col = "biom_pdl1_1_no_ALK_or_EGFR",
  ) %>%
  add_row(
    ind_sheet_bio_req = "PD-L1 TPS at least 50% AND No EGFR mutations AND No ALK mutations",
    biom_col = "biom_pdl1_50_no_ALK_or_EGFR",
  ) 
  

# Genomic data biomarkers
simple_biom_tests %<>%
  add_row(
    ind_sheet_bio_req = "No EGFR mutations AND No ALK mutations",
    biom_col = "biom_no_ALK_or_EGFR",
    test_name = 'test_biom_no_EGFR_or_ALK'
  ) %>%
  add_row(
    ind_sheet_bio_req = "EGFR exon 19 deletion|EGFR p.L858R",
    biom_col = "biom_EGFR_ex19_or_pL858R"
  ) %>%
  # removing this one for now - the label for panitumumab looks like it calls
  #   for WILD TYPE KRAS, not a mutation.
  # add_row(
  #   ind_sheet_bio_req = "KRAS",
  #   biom_col = "biom_KRAS"
  # ) %>% 
  add_row(
    ind_sheet_bio_req = "BRAF p.V600E",
    biom_col = "biom_BRAF_pV600E"
  ) %>%  
  add_row(
    ind_sheet_bio_req = "EGFR p.T790M",
    biom_col = "biom_EGFR_pT790M"
  )

  
  
  

 

readr::write_rds(
  simple_biom_tests,
  file = here('data', 'linked_approvals', 'simple_biom_tests.rds')
)


