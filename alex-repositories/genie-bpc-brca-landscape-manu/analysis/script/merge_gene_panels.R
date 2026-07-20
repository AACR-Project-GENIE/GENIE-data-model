library(here); library(fs); library(purrr);
purrr::walk(fs::dir_ls('R'), .f = source)

vec_gene_panels <- fs::dir_ls('data-raw') %>%
  str_filter(., 'data_gene_panel_.*')

gp_all <- purrr::map_dfr(
  .x = vec_gene_panels,
  .f = tidy_gene_panel
)

saveRDS(
  object = gp_all,
  file = here('data', 'gene_panel_all.rds')
)




# Load in the mutation data and merge/process it.
cli::cli_alert_danger(
  "Mutation data processing was a guess to get the statistical steps set up.  
  Needs to be verified by someone with expertise!"
)
dft_cpt <- readr::read_rds(
  here('data', 'clin_data_cohort', 'dft_cpt.rds')
)
dft_mut <- readr::read_tsv(here('data-raw', 'data_mutations_extended.txt'))

dft_mut %<>%
  mutate(
    sift_flag = SIFT_Prediction %in% c("deleterious", "deleterious_low_confidence"),
    polyphen_flag = Polyphen_Prediction %in% c("probably_damaging", "possibly_damaging")
  ) %>%
  filter(sift_flag | polyphen_flag) %>%
  select(-c(sift_flag, polyphen_flag))

# The tumor sample barcodes seem to match up nicely with this column:
# dft_mut$Tumor_Sample_Barcode %in% dft_cpt$cpt_genie_sample_id %>% mean

dft_mut <- left_join(
  dft_mut,
  select(dft_cpt, cpt_genie_sample_id, cpt_seq_assay_id),
  by = c(Tumor_Sample_Barcode = "cpt_genie_sample_id"),
  multiple = "error" # We expect one row per cpt_genie_sample_id.
)

dft_mut_long <- dft_mut %>%
  select(
    cpt_genie_sample_id = Tumor_Sample_Barcode, 
    hugo = Hugo_Symbol,
    cpt_seq_assay_id,
  ) 

# # Just curious.  This should give the number of deleterious mutations per sample.
# dft_mut_long %>% count(cpt_genie_sample_id, sort = T)
# # Are there samples with more than one associated panel?
# dft_mut_long %>%
#   group_by(cpt_genie_sample_id) %>%
#   summarize(num_panels = length(unique(cpt_seq_assay_id))) %>%
#   arrange(desc(num_panels))
# # No.
# # How many rows can we expect to be added per oncopanel?
# gp_all %>% count(stable_id)
# # About 400, so for 1000 tests we'd expect about 400k rows for our upcoming merge.

# First create a skeleton with one row per {panel_id, gene tested}.
dft_gene_feat <- dft_mut_long %>%
  select(cpt_genie_sample_id, cpt_seq_assay_id) %>%
  distinct(.) %>%
  left_join(
    .,
    gp_all,
    by = c(cpt_seq_assay_id = "stable_id"),
    multiple = "all", # we expect many matches for each cpt.
    unmatched = "error" # one offs could be excluded, but we expect all to have 
    # a matching panel.
  )

# bring in the positive tests.
dft_gene_feat <- dft_mut_long %>%
  select(cpt_genie_sample_id, hugo) %>%
  distinct(.) %>% # if multiple deleterious variants exist we compress to one row.
  mutate(variant = T) %>%
  left_join(
    dft_gene_feat,
    .,
    by = c("cpt_genie_sample_id", "hugo"),
    multiple = "error"
  )

dft_gene_feat <- dft_gene_feat %>%
  mutate(variant = if_else(is.na(variant), F, variant))

# # At this point we have a list of all genes tested.  variant = 0 means no
# # deleterious variants were found, variant = 1 means some were found.
# # Some genes are in more panels than others:
# dft_gene_feat %>% count(hugo) %>% arrange(desc(n))
# # We now want to populate this to include all genes tested over all panels,
# # which can be easily reversed by an analyst later if desired.

# As expected this has 400k rows now.  With >900 unique hugo, we would expect
# about 900k rows when this completion is done.
dft_gene_feat <- dft_gene_feat %>%
  complete(
    ., cpt_genie_sample_id, hugo,
    fill = list(tested = F, variant = NA, cpt_seq_assay_id = NA)
  )
                
# # Now we're able to get much more meaningful statistics about the genes.
# # For example, how many genes are tested in >70% of samples?
# dft_gene_feat %>% 
#   group_by(hugo) %>%
#   summarize(prop = mean(tested %in% T)) %>% 
#   mutate(prop_gte_70 = prop >= 0.7) %>%
#   summarize(genes_frequent = sum(prop_gte_70, na.rm = T))
# # A great minority.

# Because many of the upcoming methods require matrices,
dft_gene_feat %<>%
  mutate(
    tested = as.numeric(tested),
    variant = as.numeric(variant)
  ) %>%
  mutate(hugo = stringr::str_replace_all(hugo, "-", "_")) 

# Filter out genes with zero variance or no prevalence:
dft_gene_feat %<>%
  group_by(hugo) %>%
  mutate(
    hugo_var = var(variant, na.rm = T),
    any_pos = any(variant %in% 1)
  ) %>%
  ungroup(.) %>%
  filter(hugo_var > 0) %>%
  filter(any_pos) %>%
  select(-hugo_var, -any_pos)


readr::write_rds(dft_gene_feat, file = here('data', 'gene_feat_long.rds'))

# Imputation is needed for LASSO modeling - we do this now.
dft_genes_over_thresh <- dft_gene_feat %>%
  group_by(hugo) %>%
  mutate(
    hugo_prop_test = mean(tested %in% T),
    hugo_var = var(variant, na.rm = T)
  ) %>%
  ungroup(.) %>%
  filter(hugo_var > 0) %>%
  select(cpt_genie_sample_id, hugo, variant)

dft_genes_over_thresh_wide <- dft_genes_over_thresh  %>%
  pivot_wider(names_from = hugo, values_from = variant)

dft_wide_no_names <- select(dft_genes_over_thresh_wide,
                                     -cpt_genie_sample_id)
  
cli::cli_inform("Imputation takes some time (a few minutes on a 2021 macbook)")
# From caret package:
pc <- preProcess(
  dft_wide_no_names, 
  # as a start.  Effectively this adds 0 for all.
  # The bagImpute method doesn't really work - returns NANs.
  method = "medianImpute",
)
dft_imp <- predict(pc, dft_wide_no_names) 
# put the IDs back in:
dft_imp <- bind_cols(
  select(dft_genes_over_thresh_wide, cpt_genie_sample_id),
  dft_imp,
)

dft_imp <- dft_imp %>%
  pivot_longer(
    cols = -cpt_genie_sample_id,
    names_to = "hugo",
    values_to = "variant_imp"
  )

dft_gene_feat_imp <- left_join(
  dft_gene_feat,
  dft_imp,
  by = c("cpt_genie_sample_id", "hugo")
)

dft_gene_feat_imp %>% lapply(., (function(x) mean(is.na(x))))

# dft_gene_feat_imp %>% 
#   filter(variant %in% NA) %>%
#   pull(variant_imp) %>%
#   mean

readr::write_rds(dft_gene_feat_imp, file = here('data', 'gene_feat_long_imp.rds'))

  




 



