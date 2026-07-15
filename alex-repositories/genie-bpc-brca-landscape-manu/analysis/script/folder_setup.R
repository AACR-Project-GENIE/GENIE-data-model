# Description: Creates folders for the analysis.
# Note: This file is currently incomplete (did not think to create it at first,
#   so some earlier scripts still have directory creation commands embedded 
#   in specific scripts.

library(purrr); library(here); library(fs);
purrr::walk(.x = fs::dir_ls('R'), .f = source)

fs::dir_create(here('data', 'survival', 'v2', 'prepared_data'))
fs::dir_create(here('data', 'survival', 'drug', 'fit_outputs'))
fs::dir_create(here('data', 'survival', 'drug', 'fit_outputs', 'fit_summary'))
fs::dir_create(here('data', 'survival', 'v2', 'fit_outputs', 'fit_summary'))
fs::dir_create(here('data', 'survival', 'age'))
fs::dir_create(here('data', 'survival', 'drug', 'simple_km_met'))

fs::dir_create(here('output', 'fig', 'manu'))
