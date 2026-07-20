# Description: Filter the outputs down to oncogenic mutations and create
#   genomic features for survival analysis.  Specifically, we're shooting for the
#   genomic features which (1) split ERBB2 into mutation and CNA and (2) sort
#   all other genes according to gain or loss of function if both exist.

library(purrr); library(here); library(fs);
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_mut_onco <- readr::read_tsv(
  here('data', 'genomic', 'mut_onco.txt'),
  show_col_types = F
)
dft_cna_onco <- readr::read_tsv(
  here('data', 'genomic', 'cna_onco.txt'),
  show_col_types = F
)
dft_fus_onco <- readr::read_tsv(
  here('data', 'genomic', 'fus_onco.txt'),
  show_col_types = F
)

dft_cna_raw <- readr::read_tsv(
  here('data', 'genomic', 'cna_ready_to_annotate.txt'),
  show_col_types = F
) 
dft_cna_onco <- bind_cols(
  # The sample ID that came back is garbage.
  select(dft_cna_onco, -SAMPLE_ID),
  select(dft_cna_raw, SAMPLE_ID = Tumor_Sample_Barcode)
) 

anno_msg_help <- function(dat) {
  dat_name <- deparse(substitute(dat))
  
  tab <- tabyl(dat, ANNOTATED) %>%
    filter(ANNOTATED) # T/F so this grabs the annotated pct line.
  
  cli::cli_alert_info(
    text = glue("{dat_name}: Of the {tab$n} rows, {round(tab$percent*100,1)}% were annotated.")
  )
}

# Just some print outs for the analyst - hoping for near 100% in all:
anno_msg_help(dft_mut_onco)
anno_msg_help(dft_cna_onco)
anno_msg_help(dft_fus_onco)

onco_count_help <- function(dat, label) {
  tabyl(dat, ONCOGENIC) %>%
    mutate(type = label) %>%
    select(type, oncogenic = ONCOGENIC, n)
}

dft_onco_impact <- bind_rows(
  onco_count_help(dft_mut_onco, "Mutation"),
  onco_count_help(dft_cna_onco, "CNA"),
  onco_count_help(dft_fus_onco, "Fusion")
)

lev_onco <- c("Oncogenic", "Likely Oncogenic",
              "Likely Neutral",
              "Inconclusive", "Unknown")

dft_onco_impact %<>%
  mutate(
    oncogenic = factor(oncogenic, levels = lev_onco),
    type = fct_inorder(type)
  )

readr::write_rds(
  x = dft_onco_impact,
  file = here('data', 'genomic', 'oncokb_impact.rds')
)









# Create an alterations dataset - one row per alteration.

dft_mut_onco_alt <- dft_mut_onco %>%
  rename_all(tolower) %>%
  mutate(alt_type = "Mutation") %>%
  select(
    sample_id = tumor_sample_barcode,
    hugo = hugo_symbol,
    alt_type,
    hgvsc,
    hgvsp,
    chromosome,
    oncogenic,
    mutation_effect,
    highest_level,
    consequence,
    variant_classification,
    variant_type,
    mutation_status
  )

dft_cna_onco_alt <- dft_cna_onco %>% 
  rename_all(tolower) %>%
  mutate(alt_type = "CNA") %>%
  select(
    sample_id,
    hugo = hugo_symbol,
    alt_type,
    cna_desc = alteration,
    oncogenic,
    highest_level,
    mutation_effect
  )

dft_fus_onco_alt <- dft_fus_onco %>% 
  rename_all(tolower) %>%
  mutate(alt_type = "Fusion") %>%
  select(
    sample_id = tumor_sample_barcode,
    hugo = hugo_symbol,
    alt_type,
    fusion_desc = fusion,
    oncogenic,
    mutation_effect,
    highest_level
  )

dft_alt <- bind_rows(
  dft_mut_onco_alt,
  dft_cna_onco_alt,
  dft_fus_onco_alt
)

# We need a unique key.  Currently a sample can have several alterations in the same #  hugo code and alteration type.  
# Initially I wanted to use the descriptions of the
#  alterations, such as the HGVSc code or the fusion description.  Because these
#  are sometimes missing I'm going to assign a number instead.  Sometimes even with 
#  a missing HGVSc&HGVSp code the variant can be annotated by oncokb - I don't 
#  want to remove those just to fit what would have been a beautiful data structure.
dft_alt %<>% 
  group_by(sample_id) %>%
  mutate(alt_seq = 1:n()) %>%
  ungroup(.) 

lev_alt_type <- c(
  "Mutation",
  "CNA",
  "Fusion"
)


# bit of cleanup
dft_alt %<>%
  mutate(
    alt_type = factor(alt_type, levels = lev_alt_type),
    oncogenic = factor(oncogenic, levels = lev_onco),
    mutation_effect = format_mutation_effect(mutation_effect),
    mut_eff_simple = format_mutation_effect_simple(mutation_effect),
    highest_level = format_highest_level(highest_level)
  ) %>%
  select(
    sample_id,
    alt_seq,
    hugo,
    alt_type,
    # features common to all alterations:
    oncogenic,
    mutation_effect,
    mut_eff_simple,
    highest_level,
    # descriptors for each alteration type:
    hgvsc,
    hgvsp,
    cna_desc,
    fusion_desc,
    # Any other data we pulled, like mutation stuff or whatever:
    everything()
  )

readr::write_rds(
  x = dft_alt,
  file = here('data', 'genomic', 'alterations.rds')
)



# Assess the oncoKB impact on individual mutations

dft_cpt <- readr::read_rds(
  here('data', 'clin_data_cohort', 'dft_cpt.rds')
)
dft_gene_test <- dft_cpt %>%
  select(
    cpt_genie_sample_id, record_id, ca_seq, cpt_seq_assay_id, 
    contains("sample_type")
  )
dft_gp_all <- readr::read_rds(
  here('data', 'genomic', 'gene_panel_all.rds')
) %>%
  mutate(hugo = as.character(hugo))

dft_gene_test %<>%
  left_join(., dft_gp_all, by = "cpt_seq_assay_id",
            relationship = "many-to-many")

# We're making bigger assumptions here:  Any panel that tests the gene
#   is assumed to cover fusions, CNAs and mutations.  In the prostate 
#   cohort we tried to be more subtle but it's probably not justified since
#.  we really have no information at all about which CNAs and fusions are covered.
dft_gene_test %<>%
  select(
    sample_id = cpt_genie_sample_id, 
    cpt_seq_assay_id, 
    hugo, 
    contains("tested")
  ) %>%
  slice(rep(1:n(), times = 3)) %>%
  mutate(
    alt_type = rep(c("Mutation", "CNA", "Fusion"), each = n()/3),
    alt_type = factor(alt_type, levels = lev_alt_type)
  )

dft_gene_test %<>%
  filter(tested)

# Here we do a diversion to investigate something:  Are there samples here
#  which were NOT tested for an alteration but show it anyway.
# dft_gene_test_true <- dft_gene_test %>% filter(test_logical)
# 
# set.seed(130)
# chk_untested_but_positive <- anti_join(
#   dft_alt,
#   dft_gene_test_true,
#   by = c("sample_id", "hugo", "alt_type")
# ) # %>%
#   group_by(alt_type) %>%
#   arrange(desc(!(oncogenic %in% "Unknown"))) %>% 
#   slice(1:3) %>%
#   ungroup(.)
# 
# chk_untested_but_positive %>% 
#   slice(c(2,3,4,5,6,7)) %>%
#   select(sample_id, hugo, alt_type, oncogenic, highest_level, fusion_desc)
# 
# vec_ubp_sample_id <- chk_untested_but_positive %>%
#   filter(!(alt_type %in% "Fusion")) %>%
#   pull(sample_id) 
# dft_cpt %>% 
#   filter(cpt_genie_sample_id %in% vec_ubp_sample_id) %>% 
#   tabyl(cpt_seq_assay_id)



dft_gene_test <- dft_alt %>% 
  # everything in this dataframe represents an alteration, so:
  mutate(altered = 1) %>%
  select(sample_id, hugo, alt_type, altered, oncogenic, mut_eff_simple) %>%
  left_join(
    dft_gene_test,
    .,
    by = c("sample_id", "hugo", "alt_type")
  )

dft_gene_test %<>%
  mutate(altered = if_else(is.na(altered), 0, altered))


readr::write_rds(
  x = dft_gene_test,
  file = here('data', 'genomic', 'alt_test_full.rds')
)



# This stuff is probably not strictly needed, but it is useful:
dft_onco_imp <- dft_gene_test %>%
  group_by(hugo, alt_type) %>%
  summarize(
    n_tested = n(),
    n_altered = sum(altered, na.rm = T),
    n_oncogenic = sum(oncogenic %in% c("Likely Oncogenic", "Oncogenic"), na.rm = T),
    n_gain = sum(mut_eff_simple %in% "Gain", na.rm = T),
    n_loss = sum(mut_eff_simple %in% "Loss", na.rm = T),
    n_switch = sum(mut_eff_simple %in% "Switch", na.rm = T),
    .groups = "drop"
  )

readr::write_rds(
  x = dft_onco_imp,
  file = here('data', 'genomic', 'gene_counts.rds')
)
















