# Description: Grab all the gene panel files and create data frames
#   to organize that information. 

library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_assay_info <- readr::read_tsv(
  here('data-raw', 'genomic', 'assay_information.txt')
)

vec_gene_panels <- fs::dir_ls(here('data-raw', 'genomic')) %>%
  str_filter(., 'data_gene_panel_.*')

gp_all <- purrr::map_dfr(
  .x = vec_gene_panels,
  .f = tidy_gene_panel
)

# for consistency with clinical data (and I like the name better)
gp_all %<>% rename(cpt_seq_assay_id = stable_id)








# Merge in the sample data.
dft_cpt <- readr::read_rds(
  here('data', 'clin', 'dft_cpt.rds')
)

gp_sum <- dft_cpt %>% 
  select(cpt_genie_sample_id, record_id, cpt_number, 
         sample_type, cpt_seq_assay_id) %>%
  group_by(cpt_seq_assay_id) %>%
  summarize(
    n_pts = length(unique(record_id)),
    n_samples = n(),
    .groups = "drop"
  )

# Make sure we have a gene panel data file for each one found in the CPT data:
chk_gp_files <- all(gp_sum$cpt_seq_assay_id %in% unique(gp_all$cpt_seq_assay_id))
if (!chk_gp_files) {
  cli_abort("At least one gene panel found in the CPT data with no corresponding metadata file")
}





# get the number of genes in each panel, merge that in to the summary dataframe.
gp_sum <- gp_all %>%
  group_by(cpt_seq_assay_id) %>%
  summarize(
    # n() would also be fine here:
    n_genes = length(unique(hugo)),
    .groups = 'drop'
  ) %>%
  left_join(
    gp_sum,
    .,
    by = "cpt_seq_assay_id"
  )
gp_sum %<>% 
  arrange(desc(n_pts)) %>%
  mutate(
    cpt_seq_assay_id = forcats::fct_inorder(cpt_seq_assay_id)
  )
  
saveRDS(
  object = gp_sum,
  file = here('data', 'genomic', 'gene_panel_sum.rds')
)








# Get the information we need from the assay info.

dft_gp_meta <- dft_assay_info %>% 
  rename_all(tolower) %>%
  filter(seq_assay_id %in% unique(dft_cpt$cpt_seq_assay_id)) %>%
  select(
    seq_assay_id,
    alteration_types,
    calling_strategy
  ) %>%
  mutate(
    # for either intragenic or gene level CNAs:
    tested_cna = str_detect(alteration_types, "_cna"),
    tested_fusion = str_detect(alteration_types, "structural_variants")
  ) %>%
  select(
    panel = seq_assay_id,
    tested_cna,
    tested_fusion,
    calling_strategy
  ) %>%
  mutate(
    across(
      .cols = c(tested_cna, tested_fusion),
      .fns = as.logical
    )
  ) %>%
  arrange(panel)




gp_all %<>%
  left_join(
    ., dft_gp_meta, by = c(cpt_seq_assay_id = "panel")
  )

gp_by_gene <- gp_all %>% 
  group_by(hugo) %>%
  summarize(
    n_panels = length(unique(cpt_seq_assay_id)),
    .groups = "drop"
  ) 

# Find the proportion of samples which include testing for each gene.
gp_by_gene_samp_counts <- dft_cpt %>% 
  select(cpt_genie_sample_id, cpt_seq_assay_id) %>%
  mutate(n_samples = n()) %>%
  left_join(
    .,
    select(gp_all, cpt_seq_assay_id, hugo),
    by = "cpt_seq_assay_id",
    relationship = "many-to-many"
  ) %>%
  group_by(hugo) %>%
  summarize(
    num_samp_tested = n(),
    prop_samp_tested = n()/first(n_samples),
    .groups = "drop"
  )

gp_by_gene <- left_join(gp_by_gene, gp_by_gene_samp_counts, by = "hugo")

gp_by_gene %<>%
  arrange(desc(n_panels), hugo) %>%
  mutate(
    hugo = forcats::fct_inorder(hugo)
  )

saveRDS(
  object = gp_by_gene,
  file = here('data', 'genomic', 'gene_panel_by_gene.rds')
)









# apply the factor levels above to the "all" data:
gp_all %<>%
  mutate(
    cpt_seq_assay_id = factor(
      cpt_seq_assay_id, 
      levels = levels(gp_sum$cpt_seq_assay_id)
    ),
    hugo = factor(
      hugo, 
      levels = levels(gp_by_gene$hugo)
    )
  )

saveRDS(
  object = gp_all,
  file = here('data', 'genomic', 'gene_panel_all.rds')
)

  
