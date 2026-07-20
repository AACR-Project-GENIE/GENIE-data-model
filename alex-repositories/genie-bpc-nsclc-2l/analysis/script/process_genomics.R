library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

maf <- data.table::fread(
  here('data-raw', 'main_genie', 'data_mutations_extended.txt')
)

clin_samp <- readr::read_tsv(
  comment = '#',
  here('data-raw', 'main_genie', 'data_clinical_sample.txt')
)

ca_ind <- readr::read_csv(
  here('data-raw', 'NSCLC', '3.1-consortium', 'cancer_level_dataset_index.csv')
)

# very rough filter just to get a workable MAF size:
bpc_nsclc_samples <- clin_samp %>%
  filter(PATIENT_ID %in% ca_ind$record_id) %>%
  pull(SAMPLE_ID)

maf_relevant <- maf %>%
  filter(Tumor_Sample_Barcode %in% bpc_nsclc_samples) %>%
  filter(Hugo_Symbol %in% "KRAS") %>%
  filter(HGVSp_Short %in% "p.G12D")

fs::dir_create(path('data', 'genomic'))
readr::write_rds(
  maf_relevant,
  here('data', 'genomic', 'maf_rel.rds')
)

samp_kras_g12d <- maf_relevant %>%
  pull(Tumor_Sample_Barcode) %>%
  unique %>%
  sort

readr::write_rds(
  samp_kras_g12d,
  here('data', 'genomic', 'samp_kras_g12d.rds')
)


# Update: they took the offer to do a bit more genomic analysis, OK we can do that.

# Step 1:  Double check that all the panels used in BPC cover KRAS (likely yes).
bed <- data.table::fread(
  here('data-raw', 'main_genie', 'genie_combined.bed')
)

chk_kras_covered <- bed |>
  filter(
    SEQ_ASSAY_ID %in%
      (clin_samp %>%
        filter(SAMPLE_ID %in% bpc_nsclc_samples) %>%
        pull(SEQ_ASSAY_ID) %>%
        unique)
  ) |>
  group_by(SEQ_ASSAY_ID) |>
  summarize(kras_covered = any(Hugo_Symbol %in% "KRAS"), .groups = 'drop')

# KRAS is covered in all the panels.  Just throwing up one check to stop the script if not:
if (any(!pull(chk_kras_covered, kras_covered))) {
  cli_abort(
    "There are panels that don't cover KRAS - code needs to be reworked to get the correct denominator for those tested"
  )
}

# Find the samples that are some other sort of G12 variant:
samp_g12_other <- maf %>%
  filter(Tumor_Sample_Barcode %in% bpc_nsclc_samples) %>%
  filter(Hugo_Symbol %in% "KRAS") %>%
  filter(str_detect(HGVSp_Short, "p.G12") & !(HGVSp_Short %in% "p.G12D")) %>%
  pull(Tumor_Sample_Barcode) %>%
  unique %>%
  sort

# Incredibly there is one sample showing both:
intersect(samp_g12_other, samp_kras_g12d)

kras_groups <- tibble(
  sample_id = bpc_nsclc_samples
) %>%
  mutate(
    alt_g12d = sample_id %in% samp_kras_g12d,
    alt_g12other = sample_id %in% samp_g12_other,
    no_kras_g12 = !(alt_g12d | alt_g12other)
  )

readr::write_rds(
  kras_groups,
  here('data', 'genomic', 'kras_groups.rds')
)
