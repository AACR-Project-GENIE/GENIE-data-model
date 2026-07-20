# Desription: Takes the full list of alterations and creates sample level features.
#   This is based on the survival modeling from Sept 2023 which splits genes
#   only if there is some evidence they have a difference of effect.  We also
#   use only oncogenic features.

library(purrr); library(here); library(fs);
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

vec_split_by_type <- c("ERBB2")

dft_gp_all <- readr::read_rds(
  here('data', 'genomic', 'gene_panel_all.rds')
)

# hugo codes for genes covered in all panels
vec_hugo_ap <- dft_gp_all %>% 
  count(hugo) %>% 
  filter(n %in% max(n)) %>%
  pull(hugo)




dft_alt <- readr::read_rds(
  here('data', 'genomic', 'alterations.rds')
)

dft_alt_filt <- dft_alt %>%
  filter(oncogenic %in% c("Likely Oncogenic", "Oncogenic")) %>%
  filter(hugo %in% vec_hugo_ap)

# Dataframe for genes with a differential effect.
dft_diff_eff <- dft_alt_filt %>%
  count(hugo, mut_eff_simple) %>%
  group_by(hugo) %>%
  pivot_wider(
    names_from = mut_eff_simple,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(
    has_gain_and_loss = if_else(
      Gain >= 1 & Loss >= 1,
      T,
      F
    )
  ) %>%
  ungroup(.)

# We will probably want to share these with the group:
readr::write_rds(
  x = dft_diff_eff,
  file = here('data', 'genomic', 'gene_gof_lof.rds')
)


# This is 100% based on expert opinion and/or going with the flow:
vec_split_by_type <- c("ERBB2")

vec_split_by_function <- dft_diff_eff %>%
  filter(has_gain_and_loss & !(hugo %in% vec_split_by_type)) %>%
  pull(hugo)

dft_gene_covar <- split_gene_features(
  dat_alt = dft_alt_filt,
  vec_type = vec_split_by_type,
  # Oct 1 2023 update:  Turns out immediately after writing the code for this
  #  that the group didn't want to do it.  So no we feed in no features for 
  #  funciton splitting.
  vec_function = character(0)
)

# Quick sanity check on both the feature names and numbers:
# dft_gene_covar %>% select(-1) %>% colSums

dft_gene_covar <- readr::write_rds(
  x = dft_gene_covar,
  file = here('data', 'genomic', 'gene_feat_oncogenic.rds')
)

# Todo:  
# (1) Merge this information into the existing survival prep (#2).
# (2) Run the stuff, looking back to your promises from slides.
# (2) Do a POC with anti-HER2 therapies.

    