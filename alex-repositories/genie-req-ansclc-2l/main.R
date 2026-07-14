library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

src_help <- function(file) {
  source(here('analysis', 'script', file))
}

src_help('get_raw_data.R')
src_help('gi_to_testing_matrix.R')
src_help('make_gene_matrix.R')
src_help('pt_gene_first_sample.R')
src_help('create_cohort_part_1.R')
src_help('derive_lot.R')
src_help('create_cohort_part_2.R')

# Run the report (automatable but not at the moment)
