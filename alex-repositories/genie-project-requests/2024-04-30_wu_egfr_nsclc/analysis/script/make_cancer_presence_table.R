
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
dft_img <- readr::read_csv(
  here('data-raw', 'imaging_level_dataset.csv')
)


dft_med_onc_cancer <- dft_med_onc %>% 
  filter(str_detect(md_ca, "Yes,|No,")) %>%
  mutate(
    dob_obs_days = md_onc_visit_int, 
    source = "med_onc",
    ca_short = case_when(
      str_detect(md_ca, "No,") ~ F,
      str_detect(md_ca, "Yes,") ~ T,
      T ~ NA
    )
  ) %>%
  select(record_id, dob_obs_days, source, ca_short)

dft_img_cancer <- dft_img %>% 
  filter(str_detect(image_ca, "Yes,|No,")) %>%
  mutate(
    dob_obs_days = image_scan_int, 
    source = "img",
    ca_short = case_when(
      str_detect(image_ca, "No,") ~ F,
      str_detect(image_ca, "Yes,") ~ T,
      T ~ NA
    )
  ) %>%
  select(record_id, dob_obs_days, source, ca_short)

dft_cancer_status <- bind_rows(
  dft_med_onc_cancer,
  dft_img_cancer
)

dft_cancer_status %<>%
  group_by(record_id, dob_obs_days) %>%
  summarize(
    ca_short = case_when(
      all(ca_short %in% T) ~ T,
      all(ca_short %in% F) ~ F,
      T ~ NA # two reports on the same day which are indeterminate.
    ),
    .groups = "drop"
  )

dft_cancer_status %<>%
  filter(!is.na(ca_short))

readr::write_rds(
  x = dft_cancer_status,
  file = here('data', 'ca_evid_combined.rds')
)
