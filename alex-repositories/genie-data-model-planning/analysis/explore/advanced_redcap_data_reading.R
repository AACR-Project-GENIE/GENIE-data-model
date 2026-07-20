# Some of the problems I'm running into occur because the redcap data is being
#   interpretted as logicals when it's totally blank.  This is an exploration
#   to fix that.

library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

curated_path <- here('data-raw', 'bpc', 'step1-curated', 'NSCLC2')

dat_ex <- readr::read_csv(
  dir_ls(here(curated_path, "DFCI")),
)

dat_dict <- readr::read_rds(
  here('data', 'bpc', 'step1-curated', 'aligned_data_dictionary.rds')
)
