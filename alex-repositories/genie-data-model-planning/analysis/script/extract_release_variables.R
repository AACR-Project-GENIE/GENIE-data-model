library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dir_release <- here('data-raw', 'bpc', 'step4-release')

cohort_release <- tibble(
  cohort = dir(dir_release)
) %>%
  mutate(
    version = purrr::map(
      .x = cohort,
      .f = \(x) dir(here(dir_release, x))
    )
  ) %>%
  unnest(version) %>%
  mutate(ver_stub = stringr::str_sub(version, 1, 3))

# Some old releases have months...
cohort_release %<>%
  filter(!str_detect(ver_stub, '^[A-z]')) %>%
  separate(ver_stub, into = c('major', 'minor'), sep = '\\.')

cohort_release %<>%
  group_by(cohort) %>%
  arrange(desc(major), desc(minor)) %>%
  slice(1) %>%
  ungroup

cohort_release %<>%
  mutate(
    file = purrr::map2(
      .x = cohort,
      .y = version,
      .f = \(c, v) dir(here(dir_release, c, v))
    )
  ) %>%
  unnest(file) %>%
  mutate(path = here(dir_release, cohort, version, file))

cohort_release <- cohort_release %>%
  mutate(
    dat = purrr::map(
      .x = path,
      .f = \(x) readr::read_csv(file = x)
    )
  )

cohort_release <- cohort_release %>%
  mutate(
    var_dat = purrr::map(
      .x = dat,
      .f = var_extractor
    )
  )
# Just a comment on the absurdity of our REDcap outputs:  this is 254 Mb and
#   the redcap data (no derived vars was 5 Gb.  Why hasn't this been fixed?

cohort_release %<>%
  file_2_short(.) %>%
  select(cohort, version, table, var_dat)

readr::write_rds(
  cohort_release,
  here('data', 'bpc', 'step4-release', 'vardata_nested.rds')
)
