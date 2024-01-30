# The top-level file for this messy project.

library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

# Workflow:
source(here('analysis', 'script', 'create_folders.R'))
source(here('analysis', 'script', 'get_raw_data.R'))
source(here('analysis', 'script', 'declare_vars.R'))
source(here('analysis', 'script', 'reconstruct_ages.R'))


