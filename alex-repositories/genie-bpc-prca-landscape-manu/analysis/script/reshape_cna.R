# Description: Makes a non-matrix version of the CNA file to be input to OncoKB Annotator.
# Author: Alex Paynter

library(purrr)
library(here)
library(fs)

purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_cna <- readr::read_tsv(
  file = here("data-raw", "genomic", "data_CNA.txt")
)

dft_cna_long_selected <- dft_cna %>% 
  pivot_longer(
    cols = -Hugo_Symbol,
    names_to = "SAMPLE_ID",
    values_to = "value"
  ) %>%
  # The breast group kept only value = 2, which represents a very high 
  #   degree of amplification.  I'm keeping more data in for now and we can 
  #   easily filter this down later on.
  filter(!is.na(value) & abs(value) >= 1)

readr::write_tsv(
  x = dft_cna_long_selected,
  file = here('data', 'genomic', 'cna_long_selected.txt')
)
