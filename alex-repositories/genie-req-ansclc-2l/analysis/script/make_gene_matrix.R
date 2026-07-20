library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

mut <- data.table::fread(
  here('data-raw', 'main_genie', 'data_mutations_extended.txt')
)

mut %<>% rename_all(tolower)

onco_gene_feat <- mut %>%
  select(
    sample_id = tumor_sample_barcode,
    hugo_symbol,
    hgvsp_short
  ) %>%
  group_by(sample_id, hugo_symbol) %>%
  summarize(altered = T, .groups = 'drop')

# We did some light sample and heavy gene selection here already - we'll use that:
testing_mat <- readr::read_rds(
  here('data', 'genomic', 'matrix_testing_by_hugo.rds')
)

onco_gene_feat %<>%
  filter(sample_id %in% rownames(testing_mat)) %>%
  filter(hugo_symbol %in% colnames(testing_mat))


# This object is larger, but we'll be able to save memory later on:
onco_gene_feat_mat <- long_dat_to_mat_helper(onco_gene_feat)
onco_gene_feat_mat[is.na(onco_gene_feat_mat)] <- FALSE

onco_comb_mat <- combine_testing_and_alteration_matrices(
  testing_mat,
  onco_gene_feat_mat
)


# Need a feature for KRAS G12C speicfically too:
custom_features <- mut %>%
  filter(hugo_symbol %in% 'KRAS' & hgvsp_short %in% "p.G12D") %>%
  select(sample_id = tumor_sample_barcode) %>%
  mutate(KRAS_G12D = T) %>%
  left_join(
    tibble(sample_id = rownames(onco_comb_mat)),
    .,
    by = 'sample_id'
  )

onco_comb_df <- full_join(
  as_tibble(onco_comb_mat, rownames = 'sample_id'),
  custom_features,
  by = 'sample_id'
)

onco_comb_df %<>%
  mutate(
    KRAS_G12D = case_when(
      is.na(KRAS) ~ NA,
      # if kras is true or false the gene was tested.
      KRAS %in% c(TRUE, FALSE) & KRAS_G12D %in% NA ~ FALSE,
      T ~ KRAS_G12D
    )
  )

# I think it'll be useful to have record id in here too:

onco_comb_df %<>%
  left_join(
    .,
    select(samp, sample_id, record_id = patient_id),
    by = 'sample_id'
  ) %>%
  relocate(record_id, .after = sample_id)

readr::write_rds(
  onco_comb_df,
  here('data', 'genomic', 'sample_gene_test_pos.rds')
)
