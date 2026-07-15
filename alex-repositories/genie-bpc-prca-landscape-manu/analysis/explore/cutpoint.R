# This script finds the optimal cutpoint of oncogenic TMB (TOMB).



library(purrr);
library(here)
library(fs)

purrr::walk(.x = fs::dir_ls(here('R')), .f = source) 

dft_gp_all <- read_wrap_geno('gene_panel_all.rds')
dft_cpt <- read_wrap_clin("dft_cpt_aug.rds")

library(cutpointr)


dft_cut_test <- dft_cpt %>%
  mutate(
    hypermutated_raw = if_else(
      tmb_Mb > 10,
      1, 0, NA_real_
    )
  )

dft_cut_test <- dft_gp_all %>%
  select(cpt_seq_assay_id, calling_strategy) %>%
  distinct %>%
  left_join(
    dft_cut_test, 
    .,
    by = 'cpt_seq_assay_id'
  ) 

dft_cut_test %<>% 
  filter(calling_strategy %in% "tumor_normal") %>%
  select(
    record_id, 
    institution,
    cpt_seq_assay_id,
    calling_strategy,
    tmb_Mb,
    tmb_Mb_onco,
    hypermutated_raw
  )


mod <- cutpointr::cutpointr(
  data = dft_cut_test,
  x = tmb_Mb_onco,
  class = hypermutated_raw,
  metric = accuracy,
  # metric = cohens_kappa
  # metric = F1_score
  # metric = sum_sens_spec
) 



mod$optimal_cutpoint

# always worth a look:
plot_metric(mod) +
  coord_cartesian(xlim = c(-0.1, 15))


vec_prop_hyper_raw <- dft_cut_test %>%
  summarize(prop_hyper_raw = mean(hypermutated_raw)) %>%
  pull(prop_hyper_raw)

quantile(dft_cut_test$tmb_Mb_onco, 1-vec_prop_hyper_raw)





  
    



