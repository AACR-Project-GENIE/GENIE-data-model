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
  "Same set of record_id's in pt table",
  expect_setequal(tables_leg$pt$record_id, tables_new$pt$record_id)
)

cli::cli_inform(
  "Columns being checked in pt: {.code tables_leg$pt}: {.val {names(tables_leg$pt)}}"
)

print(
  waldo::compare(
    arrange(tables_leg$pt, record_id),
    arrange(tables_new$pt, record_id)
  )
)
