# Description: Creates any folders needed for project.
# Author: Alex Paynter

library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

# There is some data-derived setup in get_raw_data.R too.

fs::dir_create(here('output', 'img'))
fs::dir_create(here('data', 'no_ca_seq_filter'))
fs::dir_create(here('data', 'warner_materials'))
fs::dir_create(here('data', 'linked_approvals'))
fs::dir_create(here('data', 'linked_approvals', 'stepwise'))

fs::dir_create(here('data', 'cohort', 'biomarker_flags'))
