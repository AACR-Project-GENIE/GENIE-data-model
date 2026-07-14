# This script will be used for checking our derived variable code against previous work.

library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)
script_runner <- function(
  name
) {
  source(here('analysis', 'script', 'dv_check', name))
  invisible(name)
}

# just commenting this out while they aren't changing.
# all script_runner() lines are absolutely essential and should be run as a part of this workflow.
# script_runner('align_dd_dv_check.R')
# script_runner('split_all_redcaps.R')
script_runner('derive_all_tables.R')
script_runner('convenience_restrict_patients.R')

# script_runner('dv_check_pt.R')
script_runner('dv_check_ca_ind.R')
# script_runner('dv_check_reg.R')
# script_runner('dv_check_cpt.R')
# script_runner('dv_check_img.R')
# script_runner('dv_check_path.R')
