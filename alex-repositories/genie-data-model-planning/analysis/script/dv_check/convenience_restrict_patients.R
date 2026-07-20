library(geniedv)
library(fs)
library(purrr)
purrr::walk(.x = fs::dir_ls(fs::path("R")), .f = source)

# For each (table_name = key_cols) entry in `spec`, restrict tables_1[[name]]
# and tables_2[[name]] to rows whose key tuple (1) appears in both tables and
# (2) appears exactly once in each. Returns both tables objects in a list.
tables_new <- readr::read_rds(here(path(
  'data',
  'dv',
  'layer_2_derived_tables',
  'table_list.rds'
)))

dir_nsclc_3.1 <- here(
  'data-raw',
  'bpc',
  'step4-release',
  'NSCLC',
  '3.1-consortium'
)

legacy_reader <- function(file) {
  readr::read_csv(
    here(dir_nsclc_3.1, file)
  ) %>%
    filter(phase %in% "Phase II")
}

tables_legacy <- list(
  pt = legacy_reader('patient_level_dataset.csv'),
  ca_ind = legacy_reader('cancer_level_dataset_index.csv'),
  ca_non_ind = legacy_reader('cancer_level_dataset_non_index.csv'),
  reg = legacy_reader('regimen_cancer_level_dataset.csv'),
  rad = legacy_reader('ca_radtx_dataset.csv'),
  path = legacy_reader('pathology_report_level_dataset.csv'),
  img = legacy_reader('imaging_level_dataset.csv'),
  med_onc = legacy_reader('med_onc_note_level_dataset.csv'),
  cpt = legacy_reader('cancer_panel_test_level_dataset.csv')
)


restricted <- restrict_to_shared_patients(tables_legacy, tables_new)
tables_legacy <- restricted$tables_legacy
tables_new <- restricted$tables_new

leg_all_cancers <- bind_rows(
  tables_legacy$ca_ind,
  tables_legacy$ca_non_ind
)

leg_all_cancers %>%
  count(
    record_id,
    dob_ca_dx_days,
    naaccr_laterality_cd,
    naaccr_clin_stage_cd,
    sort = T
  ) %>%
  filter(n >= 2)

tables_new$ca_ind$naaccr_laterality_cd
tables_legacy$ca_ind$naaccr_laterality_cd

# paranoid so converting types before sending into hash algo.
tables_new$ca_ind %<>%
  mutate(naaccr_laterality_cd = as.numeric(naaccr_laterality_cd))
tables_new$ca_non_ind %<>%
  mutate(naaccr_laterality_cd = as.numeric(naaccr_laterality_cd))


tables_new$ca_ind %<>% add_cancer_hash()
tables_new$ca_non_ind %<>% add_cancer_hash()
tables_legacy$ca_ind %<>% add_cancer_hash()
tables_legacy$ca_non_ind %<>% add_cancer_hash()


cancer_hashes_new <- c(
  tables_new$ca_ind$cancer_hash,
  tables_new$ca_non_ind$cancer_hash
)

cancer_hashes_legacy <- c(
  tables_legacy$ca_ind$cancer_hash,
  tables_legacy$ca_non_ind$cancer_hash
)

convenient_hashes <- intersect(
  vec_unique_only(cancer_hashes_new),
  vec_unique_only(cancer_hashes_legacy)
)

tables_new <- restrict_by_ca_hash(tables_new, convenient_hashes)
tables_legacy <- restrict_by_ca_hash(tables_legacy, convenient_hashes)

tables_new <- remap_ca_seq(tables_new, ref_tables = tables_legacy)

tab_list <- restrict_to_shared_unique(
  tables_new,
  tables_legacy,
  spec = list(
    reg = c("record_id", "ca_seq", "drugs_startdt_int_1"),
    cpt = c('record_id', 'ca_seq', 'dob_cpt_report_days')
  )
)

tables_new <- tab_list[[1]]
tables_legacy <- tab_list[[2]]


dir_out <- here('data', 'dv', 'layer_2_derived_tables')
readr::write_rds(tables_legacy, path(dir_out, 'comparable_tables_legacy.rds'))
readr::write_rds(tables_new, path(dir_out, 'comparable_tables_new.rds'))
