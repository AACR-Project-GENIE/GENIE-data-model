library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

vardata_release <- readr::read_rds(
  here('data', 'bpc', 'step4-release', 'vardata_nested.rds')
)

varsum_release <- vardata_release %>%
  unnest(var_dat) %>%
  group_by(var) %>%
  summarize(
    in_release = TRUE,
    n_tables_release = length(unique(table)),
    tables_release = paste(sort(unique(table)), collapse = ', '),
    n_cohorts_release = length(unique(cohort)),
    cohorts_release = paste(sort(unique(cohort)), collapse = ', '),
    total_missing_release = sum(n_missing),
    max_n_vals_release = max(n_vals, na.rm = T),
    .groups = 'drop'
  )

vardata_merged <- readr::read_rds(
  here('data', 'bpc', 'step2-merged', 'vardata.rds')
)
# Because our interest is in the latest release, and NSCLC2 becomes NSCLC 3.1 by the time of the release (note the numeric problems), we'll do some manual renaming here.
vardata_merged <- vardata_merged %>%
  filter(
    cohort %in%
      c(
        'BLADDER',
        'BrCa',
        'CRC2',
        'NSCLC2',
        'PANC',
        'Prostate'
      )
  ) %>%
  mutate(
    cohort = case_when(
      cohort %in% "NSCLC2" ~ "NSCLC",
      cohort %in% "CRC2" ~ "CRC",
      T ~ cohort
    )
  )

varsum_merged <- vardata_merged %>%
  group_by(var) %>%
  summarize(
    in_merged = TRUE,
    n_cohorts_merged = length(unique(cohort)),
    cohorts_merged = paste(sort(unique(cohort)), collapse = ', '),
    .groups = 'drop'
  )

varsum <- full_join(
  varsum_release,
  varsum_merged,
  by = 'var'
)

varsum %<>%
  replace_na(
    list(in_release = F, in_merged = F)
  ) %>%
  select(var, in_release, in_merged, everything())

readr::write_rds(
  varsum,
  here('data', 'bpc', 'multiple', 'var_release_vs_merged.rds')
)
