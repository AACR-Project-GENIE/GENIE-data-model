
library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_all <- readr::read_rds(
  here('data', 'all_dat.rds')
)

get_var_dat <- function(dat) {
  tibble(
    var = names(dat),
    type = purrr::map_chr(dat, typeof)
  )
}

# dft_all %>%
#     filter(dat_name %in% 'ca_ind') %>%
#     filter(cohort %in% "CRC") %>%
#     pull(dat) %>%
#     `[[`(.,1) %>%
#     get_var_dat

dft_all %<>%
  mutate(
    var_dat = map(
      .x = dat,
      .f = get_var_dat
    )
  )

dft_var <- dft_all %>%
  select(cohort, dat_name, var_dat) %>%
  unnest(var_dat)

dft_unique_var <- dft_var %>%
  group_by(var, type) %>%
  slice(1) %>%
  ungroup(.)

dft_unique_var %>% View(.)


dft_var %>%
  group_by(dat_name, cohort) %>%
  summarize(
    has_ca_seq = any(var %in% "ca_seq"),
    .groups = "drop"
  ) %>%
  print(n=500)
  
  
  
  
  
# Keep an eye on: 
# - in pt we have birth_year
# - in cpt we have cpt_seq_date (actually a year)
  


