
library(purrr); library(here); library(fs);
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_alt <- readr::read_rds(
  here('data', 'genomic', 'alterations.rds')
)

dft_alt %>%
#  filter(oncogenic %in% c("Likely Oncogenic", "Oncogenic")) %>%
  filter(hugo %in% "JAK2") %>%
  View(.)
