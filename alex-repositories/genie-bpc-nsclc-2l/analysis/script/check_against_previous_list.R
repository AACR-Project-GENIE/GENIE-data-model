# This script does not contribute to the main workflow, it just checks the list we have against a previous list of KRAS G12D cases to make sure we didn't miss anything.

library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

prev_list <- readr::read_tsv(
  here('data-raw', '20260309_revmed_nsclc_genie_bpc_3_1_clinical_data.tsv')
)

prev_list %<>%
  select(`Study ID`, `Patient ID`, `Sequence Assay ID`)

flow_track <- readr::read_rds(here('data', 'flow_track.rds'))
my_list <- flow_track %>%
  filter(message %in% 'KRAS G12D') %>%
  pull(pt_dat) %>%
  .[[1]]

setdiff(my_list$record_id, prev_list$`Patient ID`)

setdiff(prev_list$`Patient ID`, my_list$record_id)
