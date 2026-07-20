# Description: Top level workflow for the project.  Copied mostly from the
#   BLADDER cohort.
# Author: Alex Paynter

library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

############
# Clinical #
############
source(here('analysis', 'script', 'folder_setup.R'))
source(here('analysis', 'script', 'get_raw_data.R'))
source(here('analysis', 'script', 'create_cohort_data.R'))
source(here('analysis', 'script', 'create_drug_dat.R'))