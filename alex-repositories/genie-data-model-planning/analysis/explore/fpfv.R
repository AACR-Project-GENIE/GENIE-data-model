library(geniedv)
library(fs)
library(purrr)
purrr::walk(.x = fs::dir_ls(fs::path("R")), .f = source)

multi_tab_new <- readr::read_rds(
  here('data', 'dv', 'layer_2_derived_tables', 'multisite_tables.rds')
)
