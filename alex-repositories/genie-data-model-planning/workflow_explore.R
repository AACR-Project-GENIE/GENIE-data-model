# Main workflow for the project.

library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

source(here('analysis', 'script', 'create_folders.R'))
source(here('analysis', 'script', 'get_raw_data.R')) # takes a while to run.
source(here('analysis', 'script', 'extract_merged_variables.R'))
source(here('analysis', 'script', 'extract_release_variables.R'))
# Probably a step here for comparison - forget where that was done.
