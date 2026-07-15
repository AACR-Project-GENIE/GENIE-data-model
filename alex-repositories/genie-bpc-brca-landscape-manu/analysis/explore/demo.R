# Description: Demographic tables were done by the time I was involved with the
#   breast cancer manuscript.  Need to look at the demos for modeling though, 
#   and this script helps me to do that.

library(fs); library(purrr); library(here);
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

dft_pt <- readr::read_rds(
  here('data', 'clin_data_cohort', 'dft_pt.rds')
)
dft_ca_ind <- readr::read_rds(
  here('data', 'clin_data_cohort', 'dft_ca_ind.rds')
)

dft_demo <- full_join(
  select(dft_ca_ind, record_id, bca_subtype_f_simple),
  select(dft_pt, record_id, matches("naaccr_"), institution),
  by = "record_id"
)

dft_demo %>%
  tabyl(
    institution
  )

dft_demo %>%
  tabyl(
    naaccr_race_code_primary
  )

dft_demo %>%
  tabyl(
    naaccr_race_code_secondary # nothing here.
  )

dft_demo %>%
  tabyl(
    naaccr_sex_code
  )

dft_demo %>%
  tabyl(
    naaccr_ethnicity_code
  )



dft_demo %>%
  tabyl(
    naaccr_race_code_primary,
    bca_subtype_f_simple
  )

dft_demo %>%
  tabyl(
    naaccr_race_code_primary,
    naaccr_ethnicity_code
  )



