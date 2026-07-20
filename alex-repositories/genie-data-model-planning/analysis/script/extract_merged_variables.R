library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dir_merged <- here('data-raw', 'bpc', 'step2-merged')

test_data <- readr::read_csv(
  here(dir_merged, 'BLADDERBPCIntake_data.csv')
)


test_data$redcap_repeat_instrument %>% unique # oh I know those
count(test_data, redcap_repeat_instrument, redcap_repeat_instance) %>%
  group_by(redcap_repeat_instrument) %>%
  arrange(desc(redcap_repeat_instance)) %>%
  slice(1) %>%
  ungroup(.)
# Ok so we get the idea - lots of entries ("repeats") for stuff like med onc, and imaging and fewer for cpt/dx.  I think I get the idea.

# Take all the "data" files (the IRR ones are just subsets with dupes) and read
merged_cohorts <- dir(dir_merged)[stringr::str_detect(
  dir(dir_merged),
  "data.csv$"
)] %>%
  tibble(file = .) %>%
  mutate(
    path = here(dir_merged, file),
    cohort = str_replace(file, "BPCIntake.*", "")
  ) %>%
  select(cohort, path)

# This takes a while because the REDcap export storage format is pointlessly sparse.  The resulting data will be about 5GB - absolutely rediculous.
merged_cohorts <- merged_cohorts %>%
  mutate(
    dat = purrr::map(
      .x = path,
      .f = \(x) readr::read_csv(file = x)
    )
  )

lobstr::obj_size(merged_cohorts)

merged_cohorts <- merged_cohorts %>%
  mutate(
    var_dat = purrr::map(
      .x = dat,
      .f = var_extractor
    )
  )

# Now delete that monstrosity and do a gc() to clear up some RAM (I think):
merged_cohorts <- merged_cohorts %>%
  select(-dat)
gc()

merged_vardata <- merged_cohorts %>%
  select(cohort, var_dat) %>%
  unnest(var_dat)

readr::write_rds(
  merged_vardata,
  file = here('data', 'bpc', 'step2-merged', 'vardata.rds')
)
