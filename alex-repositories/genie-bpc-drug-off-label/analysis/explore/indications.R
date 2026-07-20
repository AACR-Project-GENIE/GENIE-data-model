library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_ind <- readr::read_csv(
  here('data-raw', 'manual', 'indications working copy.csv')
)

dft_ind %>% count(condition, sort = T)
