library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

public_med_onc <- readr::read_csv(
  here('data-raw', 'NSCLC', '2.0-public', 'med_onc_note_level_dataset.csv')
)

p2_med_onc <- readr::read_csv(
  here('data-raw', 'NSCLC', '3.1-consortium', 'med_onc_note_level_dataset.csv')
)

# k let's take a different approach.  Find the max and min visit released in the public for each patient:
pub_min_max <- public_med_onc %>%
  group_by(record_id) %>%
  summarize(
    pub_min_int = min(md_onc_visit_int, na.rm = T),
    pub_max_int = max(md_onc_visit_int, na.rm = T)
  )

# pull all the phase 2 cases that are in those bounds.
p2_within_public_bounds <- p2_med_onc %>%
  left_join(
    pub_min_max,
    by = 'record_id',
    relationship = 'many-to-one'
  ) %>%
  filter(
    md_onc_visit_int >= pub_min_int,
    md_onc_visit_int <= pub_max_int
  )

if (any(p2_within_public_bounds$phase != "Phase I", na.rm = F)) {
  cli_abort("Some cases are not Phase I - important to resolve")
}


p2_within_public_bounds %<>%
  select(-c(pub_min_int, pub_max_int)) %>%
  augment_med_onc_imputed_ecog(., add_numeric = T)

fs::dir_create('output')
readr::write_csv(
  p2_within_public_bounds,
  here('output', 'nsclc_ecog_public_dates.csv')
)
