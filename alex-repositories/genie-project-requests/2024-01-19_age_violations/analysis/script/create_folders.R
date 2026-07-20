# Description: Creates any folders needed for project.
# Author: Alex Paynter

library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

# There is some data-derived setup in get_raw_data.R too.

fs::dir_create(here('output'))
fs::dir_create(here('data'))
fs::dir_create(here('data-raw'))
               
