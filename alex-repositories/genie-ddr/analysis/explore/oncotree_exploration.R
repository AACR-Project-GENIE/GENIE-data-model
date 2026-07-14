# This file started from ddr_derive.R, figured out some cool stuff with oncotree that I wanted to save.
library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

samp_aug_fs <- readr::read_rds(
  here('data', 'combined', 'samp_aug_first_sample.rds')
)

custom_ddr_list <- c(
  'ERCC2',
  'ERCC5',
  'BRCA1',
  'BRCA2',
  'RECQL4',
  'RAD51C',
  'ATM',
  'ATR',
  'FANCC'
) %>%
  sort

# Can check how often these were tested easily now:
# samp_aug_fs %>%
#   select(any_of(custom_ddr_list)) %>%
#   purrr::map(
#     .x = .,
#     .f = ~sum(is.na(.x))
#   )

# We'll calculate this as a vector then put it back in.
# rowSums() is just a huge pain with pipes, but it's fast.
any_ddr <- samp_aug_fs %>%
  select(any_of(custom_ddr_list)) %>%
  rowSums(., na.rm = T) %>%
  magrittr::is_greater_than(., 0)
samp_aug_fs %<>% mutate(any_ddr = any_ddr)

library(mskcc.oncotree)
tumor_types <- get_tumor_types()

tumor_types %>% count(parent, sort = T)
tumor_types %>% filter(oncotree_code %in% "SOFT_TISSUE")

# the website shows SOFT_TISSUE -> LIPO -> DDLS
# level is the depth on the tree.
tumor_types %>% filter(oncotree_code %in% "DDLS") %>% glimpse

tumor_types %>% count(oncotree_main_type) # 120 "main types"
tumor_types %>% count(tissue, oncotree_main_type) %>% View(.)
tumor_types %>% count(tissue)

# We can see that "cancer type" is just exactly this.
# Except for UNKNOWN, which we fill in apparently.  Oncotree has
#   cancer of unknown primary which is a bit weird.
setdiff(
  (samp_aug_fs %>%
    pull(cancer_type) %>%
    unique),
  unique(tumor_types$oncotree_main_type)
)

# CUP is mostly mets, UNKNOWN is mostly primaries so there's some intentionality on the difference:
samp_aug_fs %>%
  filter(oncotree_code %in% c("UNKNOWN", "CUP", "CUPNOS")) %>%
  tabyl(sample_type, oncotree_code) %>%
  adorn_totals(where = 'row')

samp_aug_fs %>%
  filter(sample_type %in% "Primary" & oncotree_code %in% c("CUP", "CUPNOS")) %>%
  separate(patient_id, sep = '-', into = c('junk', 'site'), extra = 'drop') %>%
  count(site)

samp_aug_fs %>% count(tissue, cancer_type, sort = F) %>% View(.)
