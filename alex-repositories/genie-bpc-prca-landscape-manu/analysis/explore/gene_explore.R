library(httr2)
library(readr)
library(here)
library(dplyr)
library(tidyr)
library(magrittr)

dft_mut <- readr::read_tsv(
  file = here("data-raw", "genomic", "data_mutations_extended.txt")
)

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


dft_fus <- readr::read_tsv(
  file = here('data-raw', 'genomic', 'data_fusions.txt')
)


dft_cna_onco <- readr::read_tsv(
  file = here("data", "genomic", "cna_long_selected_onco.txt")
)


dft_fus_onco <- readr::read_tsv(
  file = here('data', 'genomic', 'fus_onco.txt')
)


dft_cna_long %>% tabyl(value)
dft_cna_hg19 <- readr::read_tsv(
  file = here('data-raw', 'genomic', 'data_cna_hg19.seg')
)

dft_cna_hg19 %>% glimpse

dft_mut %>% 
  # Not i
  # reference genome probably constant (GRCh37), as is tumor type. 
  select(
    Hugo_Symbol,
    Entrez_Gene_Id
  )
    # reference genome probably constant (GRCh37), as is tumor type. 


dft_mut <- readr::read_tsv(
  file = here("data-raw", "genomic", "data_mutations_extended.txt")
)

dft_mut %>% 
  head(50) %>%
  readr::write_tsv(
    x = .,
    file = here('data', 'maf_test.txt')
  )

dft_mut_onco <- readr::read_tsv(
  file = here("data", "maf_test_onco.txt")
) 

dft_mut_onco %>% tabyl(., VARIANT_IN_ONCOKB)
