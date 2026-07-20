# Description:  Combine the gene features (by sample) into one RDS file.
#   We'll save wide and long versions.

library(magrittr)
library(dplyr)
library(janitor)
library(yaml)
library(here)
library(purrr)
library(fs)
library(ggplot2)
library(tidyr)
library(stringr)


purrr::walk(.x = fs::dir_ls('R'), .f = source)

dft_mut <- readr::read_rds(
  here('data', 'msk_box_derived', 'mut_reshape.rds')
)
dft_cna <- readr::read_rds(
  here('data', 'msk_box_derived', 'cna_reshape.rds')
)
dft_fus <- readr::read_rds(
  here('data', 'msk_box_derived', 'fus_reshape.rds')
)
  

rename_help <- function(x, suffix) {
  # need to do this so some of the stat functions don't throw errors:
  x <- stringr::str_replace_all(x, "-", "_")
  x <- paste0(x, suffix)
  return(x)
}

# Give these names so we can tell the difference between alteration types:
dft_mut %<>% 
  rename_at(
    .vars = vars(-stable_id),
    .funs = ~rename_help(., suffix = "_mut")
  ) 
dft_cna %<>% 
  rename_at(
    .vars = vars(-stable_id),
    .funs = ~rename_help(., suffix = "_cna")
  )    
dft_fus %<>% 
  rename_at(
    .vars = vars(-stable_id),
    .funs = ~rename_help(., suffix = "_fus")
  )    

dft_gene_feat <- full_join(dft_mut, dft_cna, by = "stable_id") %>%
  full_join(., dft_fus, by = "stable_id") %>%
  rename(cpt_genie_sample_id = stable_id)

dft_gene_feat_wide <- dft_gene_feat

# Go long - builds on previous code (starting from scratch we'd never do this)
dft_gene_feat %<>%
  pivot_longer(
    cols = -cpt_genie_sample_id,
    names_to = "feature",
    values_to = "value"
  )

# Save these - they will be filtered and merged with clinical features later on.
readr::write_rds(
  x = dft_gene_feat,
  file = here('data', 'gene_feat_long.rds')
)

readr::write_rds(
  x = dft_gene_feat_wide,
  file = here('data', 'gene_feat_wide.rds')
)
