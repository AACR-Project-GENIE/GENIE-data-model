# Description: Create the survival dataset indexing from the time of 
#   distant metastasis.
# "2" in the title here refers to the fact that we're loading the gof/lof and
#   erbb2 gene features, which has a different processing pipline.

library(purrr); library(fs); library(here);
purrr::walk(.x = fs::dir_ls('R'), .f = source)


dft_ca_ind <- readr::read_rds(
  here('data', 'clin_data_cohort', 'dft_ca_ind.rds')
)
dft_gene_feat_wide <- readr::read_rds(
  here('data', 'genomic', 'gene_feat_oncogenic.rds')
)
# Clinical characteristics already filtered and merged:
dft_clin_char <- readr::read_rds(
  here(
    'data', 'survival', 'v2', 'prepared_data',
    'clin_char.rds'
  )
)
dft_cpt_dmet <- readr::read_rds(
  here(
    'data', 'survival', 'v2', 'prepared_data',
    'cpt_dmet.rds'
  )
)
# for tracking the flow of subjects:
dft_surv_consort <- readr::read_rds(
  here(
    'data', 'survival', 'v2', 'prepared_data',
    'surv_consort.rds'
  )
)



#####################################################
### Combine gene features with relevant NGS tests ###
#####################################################

dft_gene_feat_wide %<>% 
  rename(cpt_genie_sample_id = sample_id) 

dft_gene_comb_all <- combine_cpt_gene_feat_wide(
  dat_cpt = dft_cpt_dmet, 
  dat_gene_feat_wide = dft_gene_feat_wide
)


#######################################
### Combine features, add surv vars ###
#######################################

# Clinical features are added to the keys in the gene feature dataset.
dft_dmet_surv_all <- combine_clin_gene(
  dat_gene = dft_gene_comb_all,
  dat_clin = dft_clin_char
) 


# Function only used once - added for clarity.
add_specific_dmet_vars <- function(dat) {
  dat %<>% mutate(
    dat,
    tt_cpt_dmet_yrs = case_when(
      is.na(dx_to_dmets_yrs) ~ NA_real_,
      T ~ dx_cpt_rep_yrs - dx_to_dmets_yrs,
    ),
    # glmnet won't take negative times:
    tt_cpt_dmet_yrs_pos = if_else(tt_cpt_dmet_yrs < 0, 0, tt_cpt_dmet_yrs)
  )
  return(dat)
}

dft_dmet_surv_all %<>%
  add_specific_dmet_vars(.)


dft_surv_consort <- bind_rows(
  dft_surv_consort,
  surv_cohort_track_help(dat = dft_dmet_surv_all, state = "dmet")
)

# Annoying limitation:
dft_dmet_surv_all %<>%
  filter(!(tt_cpt_dmet_yrs > tt_os_dmet_yrs))

dft_surv_consort <- bind_rows(
  dft_surv_consort,
  surv_cohort_track_help(
    dat = dft_dmet_surv_all, 
    state = "dmet, CPT <= OS follow-up"
  )
) 

dft_surv_consort %<>%
  mutate(state = forcats::fct_inorder(f = state)) %>%
  select(bca_subtype_f_simple, state, n) %>%
  arrange(bca_subtype_f_simple, state)

readr::write_rds(
  file = here('data', 'survival', 'v2', 'prepared_data', 'surv_consort_dmet.rds'),
  x = dft_surv_consort
)






# Before we filter the genes for variance/proportions, make the subgroup datasets:
dft_dmet_surv_trip_neg <- dft_dmet_surv_all %>%
  filter(bca_subtype_f_simple %in% "Triple Negative") %>%
  filter_gene_features(., prop_filter = 0.02)

dft_dmet_surv_hr_pos_her2_neg <- dft_dmet_surv_all %>%
  filter(bca_subtype_f_simple %in% "HR+, HER2-") %>%
  filter_gene_features(., prop_filter = 0.02)

dft_dmet_surv_her2_pos <- dft_dmet_surv_all %>%
  filter(bca_subtype_f_simple %in% "HER2+") %>%
  filter_gene_features(., prop_filter = 0.02)

# Now we can filter the overall data as well:
dft_dmet_surv_all %<>% 
  filter_gene_features(., 0.02)

 
# Save the datasets.
readr::write_rds(
  x = dft_dmet_surv_all,
  file = here('data', 'survival', 'v2', 'prepared_data', 'surv_dmet_all.rds')
)

readr::write_rds(
  x = dft_dmet_surv_trip_neg,
  file = here('data', 'survival', 'v2', 'prepared_data', 'surv_dmet_trip_neg.rds')
)

readr::write_rds(
  x = dft_dmet_surv_hr_pos_her2_neg,
  file = here('data', 'survival', 'v2', 'prepared_data', 'surv_dmet_hr_pos_her2_neg.rds')
)

readr::write_rds(
  x = dft_dmet_surv_her2_pos,
  file = here('data', 'survival', 'v2', 'prepared_data', 'surv_dmet_her2_pos.rds')
)

