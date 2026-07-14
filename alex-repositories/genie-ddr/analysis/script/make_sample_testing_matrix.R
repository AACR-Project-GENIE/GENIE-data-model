library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

gi <- read_tsv(
  here('data-raw', 'main_genie', 'genomic_information.txt')
)

samp_aug_proto <- readr::read_rds(
  here('data', 'combined', 'samp_aug_proto.rds')
)


gi_simp <- gi %>%
  filter(includeInPanel) %>%
  group_by(SEQ_ASSAY_ID, Hugo_Symbol) %>%
  summarize(tested = T, .groups = 'drop') %>%
  rename_all(tolower)

samp_skel <- samp_aug_proto %>%
  rename_all(tolower) %>%
  select(sample_id, seq_assay_id)

if (length(setdiff(unique(samp_skel$seq_assay_id), gi_simp$seq_assay_id)) > 0) {
  cli::cli_alert_warning(
    "Some assays. in the sample file were not found in the genomic infomation (BAM) file.  This will result in these counting as 'not tested' for a gene.  Alert data team if you see this."
  )
}


# Joins are probably a memory-inefficient way to do this, but I don't care, it works for now.
samp_skel <- left_join(
  samp_skel,
  gi_simp,
  by = 'seq_assay_id',
  relationship = 'many-to-many'
)

samp_skel_mat <- samp_skel %>%
  select(-seq_assay_id) %>%
  long_dat_to_mat_helper(
    .,
    id_col = 'sample_id',
    val_col = 'tested'
  )

samp_skel_mat[is.na(samp_skel_mat)] <- FALSE

readr::write_rds(
  samp_skel_mat,
  file = here('data', 'genomic', 'matrix_testing_by_hugo.rds')
)
