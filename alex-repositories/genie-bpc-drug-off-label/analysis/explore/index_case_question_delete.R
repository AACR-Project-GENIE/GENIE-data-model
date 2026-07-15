library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_ca_non_ind <- readr::read_csv(
  here('data-raw', 'BLADDER', 'cancer_level_dataset_non_index.csv')
)
dft_cpt <- readr::read_csv(
  here('data-raw', 'BLADDER', 'cancer_panel_test_level_dataset.csv')
)


dft_ca_non_ind %>%
  count(ca_type)

dft_ca_non_ind %>%
  filter(ca_type %in% c("Bladder Cancer", "Renal Pelvis Cancer")) %>%
  select(record_id, ca_seq, ca_type) %>%
  inner_join(
    .,
    dft_cpt, 
    by = c('record_id', 'ca_seq')
  ) %>%
  glimpse
    
    