library(fs); library(purrr); library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

dft_clin_dat_wide <- readr::read_rds(
  here('data', 'no_ca_seq_filter', 'clin_dat_wide.rds')
)

dft_clin_samp <- readr::read_tsv(
  here('data-raw', 'main_genie', 'data_clinical_sample.txt'),
  comment = "#"
)

dft_maf <- data.table::fread(
  here('data-raw', 'main_genie', 'data_mutations_extended.txt')
)

dft_cna <- data.table::fread(
  here('data-raw', 'main_genie', 'data_CNA.txt')
)





# Rough trim to all people in BPC for now, even those with 2+ cancers.
dft_bpc_id <- dft_clin_dat_wide %>%
  select(ca_ind) %>%
  mutate(ca_ind = map(.x = ca_ind, .f = \(z) select(z, record_id, ca_seq))) %>%
  unnest(ca_ind) 

dft_bpc_id <- left_join(
    distinct(select(dft_bpc_id, record_id)),
    select(dft_clin_samp, record_id = PATIENT_ID, cpt_genie_sample_id = SAMPLE_ID),
    by = 'record_id',
    relationship = 'one-to-many'
  ) %>%
  select(cpt_genie_sample_id, record_id)

dft_maf %<>%
  filter(Tumor_Sample_Barcode %in% dft_bpc_id$cpt_genie_sample_id) %>% 
  # add record ids for my ease.
  left_join(
    ., 
    dft_bpc_id,
    by = c(Tumor_Sample_Barcode = "cpt_genie_sample_id")
  )

readr::write_rds(
  dft_maf,
  here('data', 'no_ca_seq_filter', 'bpc_maf.rds')
)


dft_cna_long <- dft_cna %>% 
  pivot_longer(
    cols = -Hugo_Symbol,
    names_to = "cpt_genie_sample_id",
    values_to = "value"
  )

dft_cna_long %<>%
  filter(cpt_genie_sample_id %in% dft_bpc_id$cpt_genie_sample_id)

readr::write_rds(
  dft_cna_long,
  here('data', 'no_ca_seq_filter', 'bpc_cna_long.rds')
)


dft_maf %<>%
  as_tibble(.) %>%
  filter(
    # use the cbioportal filter to start:
    !(Variant_Classification %in% c(
      "3'Flank", "3'UTR", "5'Flank", "5'UTR", "Intron", "Silent"
    ))
  ) 

dft_maf_sum <- dft_maf %>%
  group_by(record_id) %>%
  summarize(
    any_BRAF_maf = any(Hugo_Symbol %in% "BRAF", na.rm = T),
    any_ERBB2_maf = any(Hugo_Symbol %in% "ERBB2", na.rm = T)
  )

dft_cna_sum <- dft_cna_long %>%
  left_join(., dft_bpc_id, by = "cpt_genie_sample_id") %>%
  group_by(record_id) %>%
  summarize(
    any_BRAF_amp = any(Hugo_Symbol %in% "BRAF" & value %in% 2, na.rm = T),
    any_ERBB2_amp = any(Hugo_Symbol %in% "ERBB2" & value %in% 2, na.rm = T)
  )

# Currently this doesn't necessarily cover all genomic samples.  If there was
#   no row in the cna or maf it could be missing.  Fixable by merge with CPT data.
dft_gen_sum <- full_join(
  dft_maf_sum,
  dft_cna_sum,
  by = c('record_id')
)

dft_gen_sum %<>%
  mutate(
    any_BRAF = any_BRAF_maf | any_BRAF_amp,
    any_ERBB2 = any_ERBB2_maf | any_ERBB2_amp
  ) %>%
  select(record_id, any_BRAF, any_ERBB2)
  
readr::write_rds(
  dft_gen_sum,
  here('data', 'no_ca_seq_filter', 'genomic_sum.rds')
)

  





