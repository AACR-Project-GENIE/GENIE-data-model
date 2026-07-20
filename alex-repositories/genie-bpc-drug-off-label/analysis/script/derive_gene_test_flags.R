# Description: Takes the bed file (genomic information) and creates a dataset 
#   that says whether a gene was "tested for" in each sample.
# This is fairly inexact in GENIE, but probably the best we can do currently.
# Author: Alex Paynter

library(fs); library(purrr); library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

bed <- fread(
  here('data-raw', 'main_genie', 'genomic_information.txt')
)

# From what we can tell includeInPanel = FALSE regions are NOT tested for in any
#   reasonable sense.  It's something more like "a gene that was in the vendor's
#   bed file that our assay does not actually include" from the site perspective.
bed %<>%
  as_tibble(.) %>%
  mutate(includeInPanel) %>%
  rename_all(tolower)

# One row per panel and hugo symbol.
gene_test <- bed %>% 
  select(seq_assay_id, hugo_symbol) %>%
  group_by(seq_assay_id, hugo_symbol) %>%
  slice(1) %>%
  ungroup(.)

gene_test %<>%
  filter(!(hugo_symbol %in% "")) %>%
  mutate(tested = 1)

# gene_test %>% count(Hugo_Symbol, sort = T) # looks right. 

# At this point we have lots of sites in the list which will never pop up in BPC.
# Let's load all the assay IDs in so we can filter that down.
cpt_bpc_combined <- readr::read_rds(
  here('data-raw', 'clin_dat_untouched.rds')
) %>%
  filter(short_name %in% "cpt") %>%
  pull(dat) %>%
  bind_rows(.) %>%
  filter(ca_seq %in% 0) # currently doing for cohort

cpt_bpc_combined %<>%
  # fix a few of these with obvious typos...
  mutate(
    cpt_seq_assay_id = case_when(
      cpt_seq_assay_id %in% "UHN-OCA-v3" ~ "UHN-OCA-V3",
      cpt_seq_assay_id %in% "IMPACT468" ~ "MSK-IMPACT468",
      cpt_seq_assay_id %in% "IMPACT505" ~ "MSK-IMPACT505",
      cpt_seq_assay_id %in% "UHN-555-PAN-GI-v1" ~ "UHN-555-PAN-GI-V1",
      T ~ cpt_seq_assay_id
    )
  )
      
      
# genie_bpc_assays %>% count(is.na(cpt_seq_assay_id))
# I have no idea how we have 15 missing assay IDs... but ok.  It's not a whole
#   cohort with a different name.
genie_bpc_assays <- cpt_bpc_combined %>% 
  pull(cpt_seq_assay_id) %>% unique %>% 
  .[!is.na(.)]

bpc_gene_panels_missing <- genie_bpc_assays[!(genie_bpc_assays %in% gene_test$seq_assay_id)]
if (length(bpc_gene_panels_missing) > 1) {
  cli::cli_alert_warning(
    glue("There are gene panels in BPC with no representation in the genomic_information.txt file:  {paste(bpc_gene_panels_missing, collapse = ', ')}")
  )
}

gene_test %<>%
  filter(seq_assay_id %in% genie_bpc_assays)

gene_test %<>%
  tidyr::complete(
    seq_assay_id, hugo_symbol,
    fill = list(tested = 0)
  )

readr::write_rds(
  gene_test,
  here('data', 'cohort', 'biomarker_flags', 'gene_testing_by_panel.rds')
)



record_test <- cpt_bpc_combined %>% 
  select(cohort, record_id, cpt_number, ca_seq, cpt_genie_sample_id, dob_cpt_report_days, cpt_seq_assay_id) %>%
  left_join(., gene_test, by = c(cpt_seq_assay_id = 'seq_assay_id'),
            relationship = 'many-to-many')

# This is taking too long, so I'm going to just pare down to the genes we currently care about.  Easy to add a few more (or a dozen).
hugo_list <- c('EGFR', 'ERBB2', 'BRAF', 'KRAS', 'ALK', 'ROS1', 'RET')

readr::write_rds(
  hugo_list,
  here('data', 'cohort', 'biomarker_flags', 'gene_list_inclusion.rds')
)
              

record_test <- record_test %>% 
  filter(hugo_symbol %in% hugo_list) %>% 
  mutate(tested = as.logical(tested))

# For some reason summarize tries to delete rows when I filter with the untested rows, we'll try this:
record_test_pos <- record_test %>%
  group_by(cohort, record_id, hugo_symbol) %>%
  filter(tested) %>%
  arrange(dob_cpt_report_days) %>% 
  summarize(
    ever_tested = any(tested),
    dob_first_tested = first(dob_cpt_report_days),
    sample_first_tested = first(cpt_genie_sample_id),
    .groups = 'drop'
  )

record_test %<>%
  select(cohort, record_id, hugo_symbol) %>%
  distinct(.) %>%
  left_join(., record_test_pos, by = c('cohort', 'record_id', 'hugo_symbol'))

if (
  (record_test %>% 
   count(cohort, record_id, hugo_symbol) %>% 
   pull(n) %>% 
   max) > 1
) {
  cli_abort("Non-unique gene testing results for subjects - needs to be fixed.")
}

readr::write_rds(
  record_test,
  here('data', 'cohort', 'biomarker_flags', 'gene_testing_by_person.rds')
)

