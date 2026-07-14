# Description: The main flow of this request/repo is here.

library(purrr); library(fs); library(here)
purrr::walk(.x = fs::dir_ls('R'), .f = source)

source(here('analysis', 'script', 'get_raw_data.R'))
# A few pre-process steps just to make reading easier:
source(here('analysis', 'script', 'adjuvant_define.R'))
source(here('analysis', 'script', 'get_genomic_cohort.R'))
source(here('analysis', 'script', 'make_progression_event_table.R'))
source(here('analysis', 'script', 'process_ecog.R'))
# Go through the actual processing needed for the tables:
source(here('analysis', 'script', 'feas_method_broad.R'))
source(here('analysis', 'script', 'feas_method_strict_A.R'))
source(here('analysis', 'script', 'feas_method_strict_B.R'))

# After speaking with company people we had some additional proposals to work out:
source(here('analysis', 'script', 'feas_method_jj_all.R'))
source(here('analysis', 'script', 'feas_method_jj_cop_b.R'))
source(here('analysis', 'script', 'feas_method_jj_emer.R'))

# There is some useful main genie info here, but it's just two numbers 
#   which are pulled and spit into the console.
# It takes a long time to run, d/t file size so commented it out.
# source(here('analysis', 'script', 'check_egfr_main_genie.R')

# Render the following two quarto docs:
# here('analysis', 'report', '2024_05_06_nsclc_egfr.qmd')
# here('analysis', 'report', '2024_07_10_nsclc_egfr_adv_met.qmd')
