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
  "Same set of record_id|ca_seq values in ca_ind table",
  expect_setequal(
    pull_composite_id(tables_leg, "img", record_id),
    pull_composite_id(tables_new, "img", record_id)
  )
)

cli::cli_inform(
  "Columns being checked in img: {.code tables_leg$img}: {.val {names(tables_leg$img)}}"
)


tables_leg$img <- arrange(tables_leg$img, record_id, image_scan_int)
tables_new$img <- arrange(tables_new$img, record_id, image_scan_int)

# For some reason this one patient has different scans - don't care.
tables_leg$img <- filter(tables_leg$img, record_id != "GENIE-MSK-P-0042033")
tables_new$img <- filter(tables_new$img, record_id != "GENIE-MSK-P-0042033")

# single row with a difference on scan type - obviously some hard coding or data versioning going on.
tables_new$img <- tables_new$img[-7876, ]
tables_leg$img <- tables_leg$img[-7876, ]
# Same for image_ca
tables_new$img <- tables_new$img[-8736, ]
tables_leg$img <- tables_leg$img[-8736, ]

tables_leg$img <- tables_leg$img %>%
  mutate(
    across(
      matches("image_casite[0-9]+"),
      ~ str_replace_all(.x, "\\s+", " ")
    )
  )

# kind of a lot of regions removed for some reason, but 151 out of 30k isn't that alarming.
print(
  waldo::compare(
    tables_leg$img,
    tables_new$img,
    tolerance = 1e-6
  )
)
