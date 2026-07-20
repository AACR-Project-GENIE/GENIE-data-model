library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_reg_breast <- readr::read_rds(
  here('data-raw', 'BrCa', 'regimen_cancer_level_dataset.csv')
)

install.packages("synapser", repos = c("http://ran.synapse.org", "https://cloud.r-project.org"))
library(synapser)
