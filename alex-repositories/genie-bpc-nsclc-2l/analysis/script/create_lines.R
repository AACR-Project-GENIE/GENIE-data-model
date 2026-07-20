library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

bpc_dat_path <- path('data-raw', 'NSCLC', '3.1-consortium')
ca_ind <- readr::read_csv(path(bpc_dat_path, 'cancer_level_dataset_index.csv'))
reg <- readr::read_csv(path(bpc_dat_path, 'regimen_cancer_level_dataset.csv'))

# function borrowed from genie-rw-response project.
lot <- basic_line_of_therapy(
  ca_ind = ca_ind,
  reg = reg,
  verbose = TRUE,
  remove_duplicates = TRUE
)

readr::write_rds(
  lot,
  here('data', 'lot.rds')
)
