# This script will be used for checking our derived variable code against previous work.

library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

script_runner <- function(
  name
) {
  source(here('analysis', 'script', name))
  invisible(name)
}

script_runner('get_raw_data_newdata.R')
