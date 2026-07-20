# Description: Takes the MAF file (mutation information) and creates a dataset 
#   that says whether a gene was "tested for" in each sample.
# This is fairly inexact in GENIE, but probably the best we can do currently.
# Author: Alex Paynter

library(fs); library(purrr); library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

hugo_list <- readr::read_rds(
  here('data', 'cohort', 'biomarker_flags', 'gene_list_inclusion.rds')
)
maf <- fread(here('data-raw', 'main_genie', 'data_mutations_extended.txt'))

maf %<>% as_tibble(.)

# Much like with TMB, we won't count silent (aka synonymous) mutations:
maf %<>% filter(!(Variant_Classification %in% "Silent"))

cpt_bpc_combined <- readr::read_rds(
  here('data-raw', 'clin_dat_untouched.rds')
) %>%
  filter(short_name %in% "cpt") %>%
  pull(dat) %>%
  bind_rows(.)%>%
  filter(ca_seq %in% 0) # doing for cohort, mirror here.

# Only need the samples included in at least one BPC cohort:
maf %<>%
  rename(sample_id = Tumor_Sample_Barcode) %>%
  filter(sample_id %in% cpt_bpc_combined$cpt_genie_sample_id)

# First we'll just get a bulk matrix of alterations by sample:
sample_gene_alt <- maf %>%
  filter(Hugo_Symbol %in% hugo_list) %>% # just for computational ease.
  group_by(sample_id, Hugo_Symbol) %>%
  summarize(altered = T, .groups = 'drop') %>%
  pivot_wider(
    names_from = Hugo_Symbol,
    values_from = altered, 
    values_fill = F
  )
# There are some samples missing here - we will fix after merging with custom.



# Explains the difference between these: 
# maf %>%
#   filter(Hugo_Symbol %in% "EGFR") %>%
#   filter(Variant_Classification %in% "In_Frame_Del") %>%
#   tabyl(Consequence, Variant_Classification)


# In addition to those simple gene features, we will want some specific ones:
custom_gene_feat <- maf %>%
  mutate(
    EGFR_exon19del = case_when(
      !(Hugo_Symbol %in% "EGFR") ~ F,
      Exon_Number %in% "19/28" & 
        Variant_Classification %in% "In_Frame_Del" ~ T,
      T ~ F
    ),
    EGFR_pL858R = case_when(
      !(Hugo_Symbol %in% "EGFR") ~ F,
      str_detect(HGVSp_Short, "^p.L858R$") ~ T,
      T ~ F
    ),
    EGFR_pT790M = case_when(
      !(Hugo_Symbol %in% "EGFR") ~ F,
      str_detect(HGVSp_Short, "^p.T790M$") ~ T,
      T ~ F
    ),
    BRAF_pV600E = case_when(
      !(Hugo_Symbol %in% "BRAF") ~ F,
      str_detect(HGVSp_Short, "^p.V600E$") ~ T,
      T ~ F
    )
  ) %>%
  select(sample_id, EGFR_exon19del:last_col()) %>%
  group_by(sample_id) %>%
  summarize(
    across(
      .cols = EGFR_exon19del:last_col(),
      .fns = \(z) any(z)
    )
  )


gene_feat_all <- full_join(
  custom_gene_feat,
  sample_gene_alt,
  by = "sample_id"
)

# Fixing the missing samples now:
gene_feat_all <- cpt_bpc_combined %>%
  select(sample_id = cpt_genie_sample_id) %>%
  distinct(.) %>% # can have multiple reports per sample (which I don't need)
  left_join(., gene_feat_all, by = 'sample_id') %>% 
  mutate(across(.cols = -sample_id, .fns = \(z) replace_na(z, F)))

# A few checks (not comprehensive at all, just easy things)
chk_1 <- gene_feat_all %>%
  filter((EGFR_exon19del | EGFR_pL858R | EGFR_pT790M) & !EGFR)
if (nrow(chk_1) > 0) {
  cli_abort("EGFR positive for specific variants but negative overall?  Impossible.")
}
chk_2 <- gene_feat_all %>%
  select(2:last_col()) %>%
  colSums(.)
if (any(chk_2 <= 0)) {
  cli_abort("Some genes of interest are all negative - seems like an error.")
}

gene_feat_all <- cpt_bpc_combined %>% 
  select(
    sample_id = cpt_genie_sample_id,
    record_id,
    dob_cpt_report_days
  ) %>%
  # we want the first report for each person, we'll conservatively assume
  #   variants were known to the provider then:
  group_by(sample_id) %>%
  arrange(dob_cpt_report_days) %>%
  slice(1) %>%
  ungroup(.) %>%
  left_join(., gene_feat_all, by = 'sample_id')

pos_times <- gene_feat_all %>%
  pivot_longer(
    cols = EGFR_exon19del:last_col(),
    names_to = 'gene',
    values_to = 'altered'
  ) %>%
  filter(altered) %>%
  group_by(record_id, gene) %>%
  summarize(
    dob_biom_pos = min(dob_cpt_report_days),
    .groups = 'drop'
  )

pos_times_wide <- pos_times %>%
  pivot_wider(
    names_from = 'gene',
    values_from = 'dob_biom_pos'
  )

pos_times_wide <- gene_feat_all %>%
  select(record_id) %>%
  distinct(.) %>%
  left_join(., pos_times_wide, by = 'record_id')

# Bit of cleanup.
pos_times_wide %<>%
  rename_with(~paste0("dob_biom_pos_", .x), .cols = -record_id) %>%
  select(order(colnames(.))) %>%
  select(record_id, everything())


# Load in the test times and get this ready to use in a biomarker file.
test_times <- readr::read_rds(
  here('data', 'cohort', 'biomarker_flags', 'gene_testing_by_person.rds')
)

test_times %<>%
  select(record_id, hugo_symbol, dob_first_tested) %>%
  pivot_wider(
    names_from = hugo_symbol,
    values_from = dob_first_tested
  ) %>%
  rename_with(~paste0("dob_biom_test_", .x), .cols = -record_id) 

# should have the same rows in test_times and pos_times_wide now...
if (nrow(test_times) != nrow(pos_times_wide)) {
  cli_abort("Row definitions of test and positive don't match.")
}

biom_gene <- inner_join(test_times, pos_times_wide, by = 'record_id')

readr::write_rds(
  biom_gene,
  here('data', 'cohort', 'biomarker_flags', 'biom_gene_1row_per_record.rds')
)


           
  
      
    
    
