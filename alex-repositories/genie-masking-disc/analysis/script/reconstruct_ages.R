library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_all <- readr::read_rds(
  here('data', 'all_dat.rds')
)
dft_var_from_dx <- readr::read_rds(
  here('data', 'var_from_dx.rds')
)
dft_var_to_dx <- readr::read_rds(
  here('data', 'var_to_dx.rds')
)

# For the moment we'll only consider datasets that have ca_seq as a key.
# It's possible to do the others by imputing the first BPC project cancer but
#   if the datasets are done right this should be pointless (OS var alone
#   should completely cover it).
dft_all %<>%
  filter(
    dat_name %in% c(
      'ca_ind',
      'ca_non_ind',
      'cpt',
      'reg'
    )
  )

dft_test <- dft_all %>% slice(1) %>% pull(dat) %>% `[[`(.,1)

wrap_from <- function(dat) {
  get_convert_vars(
    dat = dat,
    var_dat = dft_var_from_dx,
  ) %>%
    helper_max_int(
      ., 
      new_var_name = "var_from_dx", 
      new_value_name = "from_dx_yrs"
    )
}

wrap_to <- function(dat) {
  get_convert_vars(
    dat = dat,
    var_dat = dft_var_to_dx,
  ) %>%
    helper_max_int(
      ., 
      new_var_name = "var_to_dx", 
      new_value_name = "to_dx_yrs"
    )
}

dft_all %<>%
  mutate(
    max_to = purrr::map(
      .x = dat,
      .f = wrap_to
    ),
    max_from = purrr::map(
      .x = dat,
      .f = wrap_from
    )
  )

dft_max_times <- dft_all %>%
  group_by(cohort) %>%
  summarize(
    max_to = list(bind_rows(max_to)),
    max_from = list(bind_rows(max_from))
  ) 

dft_max_times %<>%
  mutate(
    max_to = purrr::map(
      .x = max_to,
      .f = \(x) {
        x %>%
          group_by(record_id, ca_seq) %>%
          arrange(desc(to_dx_yrs)) %>%
          slice(1) %>%
          ungroup(.)
      }
    ),
    max_from = purrr::map(
      .x = max_from,
      .f = \(x) {
        x %>%
          group_by(record_id, ca_seq) %>%
          arrange(desc(from_dx_yrs)) %>%
          slice(1) %>%
          ungroup(.)
      }
    ),
  )

dft_max_times %>%
  pull(max_from) %>%
  purrr::map(
    .x = .,
    .f = \(x) { 
      count(x, record_id, ca_seq, sort = T)
    }
  )


dft_max_times %<>%
  mutate(
    all_maxes = purrr::map2(
      .x = max_to, 
      .y = max_from,
      .f = \(x,y) {
        inner_join(x,y, by = c("record_id", "ca_seq"))
      }
    )
  ) %>%
  select(cohort, all_maxes) %>%
  unnest(all_maxes)

if ((dft_max_times %>%
  count(cohort, record_id, ca_seq, sort = T) %>%
  pull(n) %>% max) > 1) {
  cli_abort("Duplicate keys in max_times - fix!")
}

dft_max_times %<>% 
  mutate(
    max_inferred_age = to_dx_yrs + from_dx_yrs
  )

dft_max_times_pt <- dft_max_times %<>%
  group_by(record_id) %>%
  arrange(desc(max_inferred_age), ca_seq) %>%
  slice(1) %>%
  ungroup(.)
  

readr::write_rds(
  dft_max_times,
  here('data', 'max_inferred_ages.rds')
)
readr::write_rds(
  dft_max_times_pt,
  here('data', 'max_inferred_ages_pt.rds')
)
