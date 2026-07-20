# Load packages and functions:
library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

source(here('analysis', 'script', 'get_raw_data.R'))
source(here('analysis', 'script', 'prepare_data_for_oncokb_annotate.R'))
# Run the shell script annotate_oncokb.sh - see the notes in there about required environment variables.
source(here('analysis', 'script', 'sample_with_clin_data.R'))

source(here('analysis', 'script', 'make_sample_testing_matrix.R'))
source(here('analysis', 'script', 'process_oncokb_output.R'))

source(here('analysis', 'script', 'ddr_plots_cancer_type_onco.R'))
source(here('analysis', 'script', 'ddr_plots_cancer_type_no_onco_filter.R'))
