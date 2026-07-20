# Here we eliminate old drugs and save the all-cancer hdrug dataset.

library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

vec_excl_drugs <- readr::read_rds(
  here('data', 'warner_materials', 'old_drugs_list.rds')
)

# Load and create the hdrug cohort data:
dft_cohort_cases <- readr::read_rds(
  here('data', 'cohort', 'clin_dat_wide.rds')
)
# Load the tracking for drugs:
dft_drug_tracking <- readr::read_rds(
  here('data', 'cohort', 'drug_tracking_01.rds')
)


# Here we eliminate the old drugs:
dft_hdrug_cohort_lim <- dft_cohort_cases %>%
  select(cohort, hdrug) %>%
  unnest(hdrug) %>%
  filter(!(agent %in% vec_excl_drugs))

# Note how many drugs were removed in this step and save:
dft_drug_tracking <- bind_rows(
  dft_drug_tracking, 
  tibble(
    step = "Remove drugs approved before Jan 1, 1997",
    drug_key = list(select(
      dft_hdrug_cohort_lim,
      cohort, record_id, ca_seq, regimen_number, drug_number, agent
    ))
  )
)
readr::write_rds(
  x = dft_drug_tracking,
  file = here('data', 'cohort', 'drug_tracking_02.rds')
)

readr::write_rds(
  x = dft_hdrug_cohort_lim,
  file = here('data', 'cohort', 'hdrug.rds')
)
