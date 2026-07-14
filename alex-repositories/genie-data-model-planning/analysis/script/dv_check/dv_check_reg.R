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

scope_dir <- here('analysis', 'script', 'dv_check', 'dv_check_scope')
tables_leg <- apply_column_scope(tables_leg, scope_dir)
tables_new <- apply_column_scope(tables_new, scope_dir)

cli::cli_inform(
  "Columns being checked in reg: {.code tables_leg$reg}: {.val {names(tables_leg$reg)}}"
)

# Fixing an obvious problem with the legacy release:  Whitespace in regimen_drugs that's been preserved:
trim_drug_spaces <- function(x) {
  stringr::str_remove_all(x, "\\s+(?=,)|\\s+$")
}
tables_leg$reg <- tables_leg$reg %>%
  dplyr::mutate(regimen_drugs = trim_drug_spaces(regimen_drugs))

# Couple column type conversions so I can check easier:
# tables_leg$reg <- tables_leg$reg %>%
#   dplyr::mutate(across(dx_drug_end_or_lastadm_int_5, as.double))

print(
  waldo::compare(
    arrange(tables_leg$reg, record_id, ca_seq, regimen_number),
    arrange(tables_new$reg, record_id, ca_seq, regimen_number),
    tolerance = 1e-6
  )
)
