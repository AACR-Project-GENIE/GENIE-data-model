# Load the new derived tables:
library(geniedv)
library(fs)
library(purrr)
purrr::walk(.x = fs::dir_ls(fs::path("R")), .f = source)

library(testthat)

restricted_tab_dir <- here('data', 'dv', 'layer_2_derived_tables')
tables_leg <- readr::read_rds(here(
  restricted_tab_dir,
  'comparable_tables_legacy.rds'
))
tables_new <- readr::read_rds(here(
  restricted_tab_dir,
  'comparable_tables_new.rds'
))

scan_first_test <- derive_scan_dmets_long(
  tables_new$img,
  tables_new$ca_ind
) %>%
  derive_scan_dmets_first()

path_first_test <- derive_path_dmets_long(
  tables_new$path,
  tables_new$ca_ind,
  cohort_ca_types = c("Non Small Cell Lung Cancer", "Lung Cancer, NOS")
) %>%
  derive_path_dmets_first()

dx_first_test <- derive_dx_dmets_long(
  tables_new$ca_ind
) %>%
  derive_dx_dmets_first(.)

combine_dmets_derivations(
  scan_first = scan_first_test,
  path_first = path_first_test,
  dx_first = dx_first_test,
  ca_ind = tables_new$ca_ind
) %>%
  #add_overall_dmets_vars(., tables_new$ca_ind) %>%
  glimpse

add_dmets_ca_ind(
  tables = tables_new,
  cohort_ca_types = c("Non Small Cell Lung Cancer", "Lung Cancer, NOS")
)
