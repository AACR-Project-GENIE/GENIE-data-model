rel_dat_dir <- here(
  'data-raw',
  'bpc',
  'step4-release',
  'NSCLC',
  '3.1-consortium'
)

ca_ind.rel <- readr::read_csv(
  here(rel_dat_dir, 'cancer_level_dataset_index.csv')
)

ca_non_ind.rel <- readr::read_csv(
  here(rel_dat_dir, 'cancer_level_dataset_non_index.csv')
)

cpt.rel <- readr::read_csv(
  here(rel_dat_dir, 'cancer_panel_test_level_dataset.csv')
)


ca_ind.rel %>%
  filter(record_id %in% 'GENIE-DFCI-117995') %>%
  glimpse

ca_non_ind.rel %>%
  filter(record_id %in% "GENIE-DFCI-117995") %>%
  glimpse
