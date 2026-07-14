# Description:  Assess the impact of oncoKB filtering, save files with
#  only the variants which pass an oncoKB pass.

library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

# here onco just refers to it having the oncogenic tag, not being limited to those alterations.
mut_onco <- readr::read_tsv(
  here('data', 'genomic', 'mut_onco.txt'),
  show_col_types = F
)

samp_aug_proto <- readr::read_rds(
  here('data', 'combined', 'samp_aug_proto.rds')
)


# Just some print outs for the analyst - hoping for near 100% in all:
genomic_anno_msg_help(mut_onco)

lev_onco <- c(
  "Oncogenic",
  "Likely Oncogenic",
  "Resistance",
  "Likely Neutral",
  "Inconclusive",
  "Unknown"
)

mut_onco %<>%
  rename_all(tolower) %>%
  mutate(
    mut_vaf = t_alt_count / (t_alt_count + t_ref_count),
    oncogenic = factor(oncogenic, levels = lev_onco)
  )

# separate script or something probably best:
#
# readr::write_tsv(
#   mut_onco,
#   here('data', 'genomic', 'mut_onco_processed.rds')
# )

onco_gene_feat <- mut_onco %>%
  filter(oncogenic %in% c('Likely Oncogenic', 'Oncogenic')) %>%
  select(
    sample_id = tumor_sample_barcode,
    hugo_symbol
  ) %>%
  group_by(sample_id, hugo_symbol) %>%
  summarize(altered = T, .groups = 'drop')

# This object is larger, but we'll be able to save memory later on:
onco_gene_feat_mat <- long_dat_to_mat_helper(onco_gene_feat)
onco_gene_feat_mat[is.na(onco_gene_feat_mat)] <- FALSE


testing_mat <- readr::read_rds(
  here('data', 'genomic', 'matrix_testing_by_hugo.rds')
)

onco_comb_mat <- combine_testing_and_alteration_matrices(
  testing_mat,
  onco_gene_feat_mat
)

rm(onco_gene_feat_mat)
gc()

onco_gene_feat <- as_tibble(onco_comb_mat, rownames = "sample_id")

samp_aug_onco <- left_join(
  samp_aug_proto,
  onco_gene_feat,
  by = 'sample_id'
)

readr::write_rds(
  samp_aug_onco,
  here('data', 'combined', 'samp_aug_onco.rds')
)

# fs = first sample
samp_aug_onco_fs <- samp_aug_onco %>%
  group_by(patient_id) %>%
  # if there are ties for both year and age we'll just
  #   pick arbitrarily (sample_id).
  arrange(seq_year, age_at_seq_report_mod, sample_id) %>%
  slice(1) %>%
  ungroup(.)

readr::write_rds(
  samp_aug_onco_fs,
  here('data', 'combined', 'samp_aug_onco_first_sample.rds')
)

rm(samp_aug_onco)
rm(samp_aug_onco_fs)
gc()


#
#
#
######################################
# Repeat for non-oncogenic filtering #
######################################

any_gene_feat <- mut_onco %>%
  # No oncogenic filter at all here
  select(
    sample_id = tumor_sample_barcode,
    hugo_symbol
  ) %>%
  group_by(sample_id, hugo_symbol) %>%
  summarize(altered = T, .groups = 'drop')

rm(mut_onco) # large memory use - don't need it now.

any_gene_feat_mat <- long_dat_to_mat_helper(any_gene_feat)
any_gene_feat_mat[is.na(any_gene_feat_mat)] <- FALSE

# Just doing the ddr genes for memory issues:
any_gene_feat_mat <- any_gene_feat_mat[,
  intersect(colnames(any_gene_feat_mat), custom_ddr_list())
]
testing_mat <- testing_mat[, custom_ddr_list()]
any_comb_mat_ddr <- combine_testing_and_alteration_matrices(
  testing_mat,
  any_gene_feat_mat
)

rm(any_gene_feat_mat)
rm(testing_mat)
gc()

any_gene_feat_ddr <- as_tibble(any_comb_mat_ddr, rownames = "sample_id")

samp_aug_any_ddr <- left_join(
  samp_aug_proto,
  any_gene_feat_ddr,
  by = 'sample_id'
)

readr::write_rds(
  samp_aug_any_ddr,
  here('data', 'combined', 'samp_aug_any_ddr.rds')
)

# fs = first sample
samp_aug_any_ddr_fs <- samp_aug_any_ddr %>%
  group_by(patient_id) %>%
  # if there are ties for both year and age we'll just
  #   pick arbitrarily (sample_id).
  arrange(seq_year, age_at_seq_report_mod, sample_id) %>%
  slice(1) %>%
  ungroup(.)

readr::write_rds(
  samp_aug_any_ddr,
  here('data', 'combined', 'samp_aug_any_ddr_first_sample.rds')
)
