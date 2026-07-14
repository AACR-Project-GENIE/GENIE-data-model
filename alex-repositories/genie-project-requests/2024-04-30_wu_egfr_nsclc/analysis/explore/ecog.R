
library(purrr); library(fs); library(here)
purrr::walk(.x = fs::dir_ls('R'), .f = source)

dft_pt <- readr::read_csv(
  here('data-raw', 'patient_level_dataset.csv')
)
dft_ca_ind <- readr::read_csv(
  here('data-raw', 'cancer_level_dataset_index.csv')
)
dft_med_onc <- readr::read_csv(
  here('data-raw', 'med_onc_note_level_dataset.csv')
)

# is each person associated with one phase?
dft_med_onc %>%
  group_by(record_id) %>%
  summarize(
    n_phases = length(unique(phase)),
    .groups = 'drop'
  ) %>%
  arrange(n_phases)

dft_med_onc %>% 
  mutate(
    .ecog_complete = !is.na(md_ecog) & !str_detect(md_ecog, "Not documented"),
    .karn_complete = !is.na(md_karnof) & !str_detect(md_karnof, "Not documented"),
    ecog_or_karn_complete = .ecog_complete | .karn_complete
  ) %>% 
  select(-phase) %>%
  left_join(
    distinct(select(dft_ca_ind, phase, record_id)),
    .,
    by = c("record_id"),
    relationship = 'one-to-many'
  ) %>%
  group_by(record_id, phase) %>%
  summarize(
    any_ecog = any(ecog_or_karn_complete, na.rm = T)
  ) %>%
  tabyl(phase, any_ecog)
  
  