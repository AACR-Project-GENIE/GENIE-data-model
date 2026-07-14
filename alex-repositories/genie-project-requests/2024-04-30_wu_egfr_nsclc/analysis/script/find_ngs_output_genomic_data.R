
library(purrr); library(fs); library(here)
purrr::walk(.x = fs::dir_ls('R'), .f = source)

dft_cpt <- readr::read_csv(
  here('data-raw', 'cancer_panel_test_level_dataset.csv')
)

dft_cohort <- readr::read_rds(
  here('data', 'final_dat_broad_method.rds')
)

# for anyone with multiple index cancers, we'll just take the first one:
dft_cohort %<>% 
  group_by(record_id) %>%
  arrange(ca_seq) %>%
  slice(1) %>%
  ungroup(.)
  
dft_pre_post <- dft_cpt %>% 
  select(record_id, dob_cpt_report_days, cpt_genie_sample_id, cpt_seq_assay_id) %>%
  filter(record_id %in% dft_cohort$record_id) %>%
  left_join(
    ., 
    select(dft_cohort, record_id, 
           # just to be clear about what this is:
           dob_first_osi_use_days = dob_reg_start_days),
    by = "record_id",
    relationship = "many-to-one"
  )

dft_pre_post %<>%
  mutate(
    .diff = dob_cpt_report_days - dob_first_osi_use_days,
    pre_osi = .diff < 0,
    post_osi = .diff >= 0
  ) %>%
  select(-.diff) 

dft_pre_post %<>%
  select(cpt_genie_sample_id,
         record_id, 
         cpt_seq_assay_id,
         dob_cpt_report_days,
         dob_first_osi_use_days,
         pre_osi,
         post_osi)

dft_subj_sum <- dft_pre_post %>%
  group_by(record_id) %>%
  summarize(
    has_pre = any(pre_osi),
    has_post = any(post_osi),
    has_both = has_pre & has_post,
    .groups = "drop"
  )

# We won't output this anywhere, just for email:
dft_subj_sum %>%
  summarize(across(matches("^has_"), sum))
  


# Subset the genomic data to only those samples:
vec_cpt_subset <- dft_pre_post$cpt_genie_sample_id

fp_gen <- here('data-raw', 'genomic')
fs::dir_create(here('output'))
fp_out <- here('output')

dft_mut <- readr::read_tsv(here(fp_gen, 'data_mutations_extended.txt'))
dft_cna <- readr::read_tsv(here(fp_gen, 'data_CNA.txt'))
dft_sv <- readr::read_tsv(here(fp_gen, 'data_sv.txt'))
dft_gen_info <- readr::read_tsv(here(fp_gen, 'genomic_information.txt'))

dft_mut %<>% filter(Tumor_Sample_Barcode %in% vec_cpt_subset)

dft_cna %<>%
  select(Hugo_Symbol, any_of(vec_cpt_subset))

dft_sv %<>% filter(Sample_Id %in% vec_cpt_subset)

dft_gen_info %<>% filter(SEQ_ASSAY_ID %in% dft_pre_post$cpt_seq_assay_id)

readr::write_tsv(dft_pre_post, here(fp_out, 'ngs_to_osi_key.txt'))
readr::write_tsv(dft_mut, here(fp_out, 'data_mutations_extended.txt'))
readr::write_tsv(dft_cna, here(fp_out, 'data_CNA.txt'))
readr::write_tsv(dft_sv, here(fp_out, 'data_sv.txt'))
readr::write_tsv(dft_gen_info, here(fp_out, 'genomic_information.txt'))


# # Optional:  Check that the outputs can be re-read sensibly:
# readr::read_tsv(here(fp_out, 'data_mutations_extended.txt')) %>% glimpse
# readr::read_tsv(here(fp_out, 'data_CNA.txt')) %>% glimpse
# readr::read_tsv(here(fp_out, 'data_sv.txt')) %>% glimpse
# readr::read_tsv(here(fp_out, 'genomic_information.txt')) %>% glimpse

