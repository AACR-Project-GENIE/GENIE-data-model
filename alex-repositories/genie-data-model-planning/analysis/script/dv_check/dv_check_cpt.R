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

test_that(
  "Same set of record_id|ca_seq values in cpt table",
  expect_setequal(
    pull_composite_id(tables_leg, "cpt", record_id, ca_seq),
    pull_composite_id(tables_new, "cpt", record_id, ca_seq)
  )
)

cli::cli_inform(
  "Columns being checked in cpt: {.code tables_leg$cpt}: {.val {names(tables_leg$cpt)}}"
)

tables_leg$cpt %<>% arrange(tables_leg$cpt, record_id, ca_seq, cpt_number)
tables_new$cpt %<>% arrange(tables_new$cpt, record_id, ca_seq, cpt_number)

print(
  waldo::compare(
    tables_leg$cpt,
    tables_new$cpt,
    tolerance = 1e-6
  )
)
