
library(purrr); library(fs); library(here)
purrr::walk(.x = fs::dir_ls('R'), .f = source)

dft_ca_ind <- readr::read_csv(
  here('data-raw', 'cancer_level_dataset_index.csv')
)
dft_path <- readr::read_csv(
  here('data-raw', 'pathology_report_level_dataset.csv')
)

dft_exc_timing <- dft_path %>%
  filter(path_proc %in% "Surgical excision") %>%
  select(record_id, path_proc_int) %>%
  group_by(record_id) %>%
  slice(1) %>%
  ungroup(.) %>%
  rename(dob_first_excision_days = path_proc_int)

# We will want this to be relative to each cancer diagnosis for later merging.
dft_exc_timing <- dft_ca_ind %>% 
  select(record_id, ca_seq, dob_ca_dx_days) %>%
  left_join(dft_exc_timing, ., by = c("record_id")) %>%
  mutate(dx_fexc_days = dob_first_excision_days - dob_ca_dx_days) 

# We can see from the label...
# https://www.accessdata.fda.gov/drugsatfda_docs/label/2024/208065s030lbl.pdf#%5B%7B%22num%22%3A6%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C0%2C668%2C0%5D
# That the only approved indication is for surgical resection specifically.
# Therefore I won't go to the trouble of looking at radiation timing, it doesn't
#.  seem relevant at all.

readr::write_rds(
  dft_exc_timing,
  here('data', 'first_excision_timing.rds')
)
