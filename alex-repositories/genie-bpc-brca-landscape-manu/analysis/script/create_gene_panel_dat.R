# Description:  Substantially simplified version of the script from the prostate
#   cohort.

library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

vec_gene_panels <- fs::dir_ls(here('data-raw', 'genomic')) %>%
  str_filter(., 'data_gene_panel_.*')

gp_all <- purrr::map_dfr(
  .x = vec_gene_panels,
  .f = tidy_gene_panel
)

# for consistency with clinical data (and I like the name better)
gp_all %<>% rename(cpt_seq_assay_id = stable_id)



saveRDS(
  object = gp_all,
  file = here('data', 'genomic', 'gene_panel_all.rds')
)

