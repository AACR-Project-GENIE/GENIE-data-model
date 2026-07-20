library(magrittr)
library(dplyr)
library(janitor)
library(yaml)
library(here)
library(purrr)
library(fs)
library(ggplot2)
library(tidyr)
library(readr)

# Downloaded from https://app.box.com/file/1176682730774
mut_msk <- read.table(
  file.path(
    "~", 
    "Downloads", 
    "MUTATION_ONCOKB_ANNOTATED_COMMON_158_GENES_ACROSS_ALL_PANELS.txt"
  ),
  row.names = 1,
  check.names = F
) 

mut_msk <- mut_msk %>% 
  as.matrix(.) %>% 
  t %>% 
  as_tibble(rownames = "stable_id")

mut_msk_genes <- colnames(mut_msk)
mut_msk_genes <- mut_msk_genes[-1]


dft_gene_feat <- readr::read_rds(
  here('data', 'gene_feat_long_imp.rds')
)

dft_hugo_stats <- dft_gene_feat %>%
  group_by(hugo) %>%
  summarize(
    prop_tested = mean(tested %in% 1),
    hugo_var = var(variant, na.rm = T),
    prop_pos = mean(variant %in% 1)
  )

vec_hugo_covariates <- dft_hugo_stats %>%
  filter(hugo_var > 0 & prop_tested > 0.8) %>%
  pull(hugo) 

setdiff(mut_msk_genes, vec_hugo_covariates)
dft_hugo_stats %>%
  filter(hugo %in% setdiff(vec_hugo_covariates, mut_msk_genes)) %>%
  arrange(desc(prop_pos))




mut_msk %<>% as_tibble(rownames = "gene")



mut_msk %>%
  select(1)
