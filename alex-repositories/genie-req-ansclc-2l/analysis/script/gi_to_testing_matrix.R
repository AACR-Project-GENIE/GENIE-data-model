library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

gi <- read_tsv(
  here('data-raw', 'main_genie', 'genomic_information.txt')
)

ca_ind <- read_csv(
  here(
    'data-raw',
    'bpc',
    'NSCLC',
    '3.1-consortium',
    'cancer_level_dataset_index.csv'
  )
)

samp <- read_tsv(
  here('data-raw', 'main_genie', 'data_clinical_sample.txt'),
  comment = '#'
)

# clin <- read_tsv(
#   here('data-raw', 'main_genie', 'data_clinical_patient.txt'),
#   comment = '#'
# )

samp %<>% rename_all(tolower)
# clin %<>% rename_all(tolower)

gi_simp <- gi %>%
  filter(includeInPanel) %>%
  group_by(SEQ_ASSAY_ID, Hugo_Symbol) %>%
  summarize(tested = T, .groups = 'drop') %>%
  rename_all(tolower)

record_id_with_any_lung <- samp %>%
  filter(cancer_type %in% 'Non-Small Cell Lung Cancer') %>%
  pull(patient_id)

samp_skel <- samp %>%
  filter(
    patient_id %in% record_id_with_any_lung | patient_id %in% ca_ind$record_id
  ) %>%
  select(sample_id, seq_assay_id, cancer_type, oncotree_code)

if (length(setdiff(unique(samp_skel$seq_assay_id), gi_simp$seq_assay_id)) > 0) {
  cli::cli_alert_warning(
    "Some assays. in the sample file were not found in the genomic infomation (BAM) file.  This will result in these counting as 'not tested' for a gene.  Alert data team if you see this."
  )
}

samp_skel <- left_join(
  samp_skel,
  gi_simp,
  by = 'seq_assay_id',
  relationship = 'many-to-many'
)

samp_skel %<>%
  filter(hugo_symbol %in% c('KRAS', 'EGFR', 'ERBB2', 'MET', 'ALK'))

samp_skel_mat <- samp_skel %>%
  select(-seq_assay_id) %>%
  long_dat_to_mat_helper(
    .,
    id_col = 'sample_id',
    val_col = 'tested'
  )

samp_skel_mat[is.na(samp_skel_mat)] <- FALSE

fs::dir_create(here('data', 'genomic'))
readr::write_rds(
  samp_skel_mat,
  file = here('data', 'genomic', 'matrix_testing_by_hugo.rds')
)
