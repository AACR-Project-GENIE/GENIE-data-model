# Description: Create the survival dataset indexing from the time of 
#   distant metastasis.

library(magrittr)
library(dplyr)
library(janitor)
library(yaml)
library(here)
library(purrr)
library(fs)

library(ggplot2)
library(ggtext)
library(ggrepel)

library(tidyr)
library(stringr)
library(survival)
library(glmnet)

purrr::walk(.x = fs::dir_ls('R'), .f = source)


# Load some data to play with:
dft_pt <- readr::read_rds(
  here('data', 'clin_data_cohort', 'dft_pt.rds')
)
dft_ca_ind <- readr::read_rds(
  here('data', 'clin_data_cohort', 'dft_ca_ind.rds')
)
dft_cpt <- readr::read_rds(
  here('data', 'clin_data_cohort', 'dft_cpt.rds')
)


dft_mut <- readr::read_rds(
  here('data', 'msk_box_derived', 'mut_reshape.rds')
)
dft_cna <- readr::read_rds(
  here('data', 'msk_box_derived', 'cna_reshape.rds')
)
dft_fus <- readr::read_rds(
  here('data', 'msk_box_derived', 'fus_reshape.rds')
)
  

rename_help <- function(x, suffix) {
  # need to do this so some of the stat functions don't freak out:
  x <- stringr::str_replace_all(x, "-", "_")
  x <- paste0(x, suffix)
  return(x)
}

# Give these names so we can tell the difference between alteration types:
dft_mut %<>% 
  rename_at(
    .vars = vars(-stable_id),
    .funs = ~rename_help(., suffix = "_mut")
  ) 
dft_cna %<>% 
  rename_at(
    .vars = vars(-stable_id),
    .funs = ~rename_help(., suffix = "_cna")
  )    
dft_fus %<>% 
  rename_at(
    .vars = vars(-stable_id),
    .funs = ~rename_help(., suffix = "_fus")
  )    

dft_gene_feat <- full_join(dft_mut, dft_cna, by = "stable_id") %>%
  full_join(., dft_fus, by = "stable_id") %>%
  rename(cpt_genie_sample_id = stable_id)

# Go long to conform with previous code.
dft_gene_feat %<>%
  pivot_longer(
    cols = -cpt_genie_sample_id,
    names_to = "feature",
    values_to = "value"
  )

# dft_gene_feat %>% lapply(., FUN = (function(x) mean(is.na(x))))



dft_dmet_timing <- get_dmet_timing(ca_ind_df = dft_ca_ind)

# Example:  Get all NGS which occur prior to metastatic cancer, or any which are
#   the first NGS test.
dft_cpt_dmet <- get_cpt_by_time(
  time_dat = dft_dmet_timing,
  time_var = "tt_y",
  cpt_dat = dft_cpt,
  always_keep_first = T
)

dft_cpt_dmet %<>%
  select(
    record_id, 
    ca_seq, 
    cpt_genie_sample_id,
    dx_cpt_rep_yrs,
    is_first_cpt,
    cpt_before_t,
    cpt_seq_assay_id
  )

dft_gene_feat_dmet <- dft_gene_feat %>%
  filter(cpt_genie_sample_id %in% unique(dft_cpt_dmet$cpt_genie_sample_id))

# Require that there is some variance in the gene test results.
dft_gene_feat_dmet %<>%   
  group_by(feature) %>%
  mutate(
    f_var = var(value, na.rm = T),
    prop_pos = mean(value %in% 1)
  ) %>%
  ungroup(.) %>%
  # 0.5% of dmet cases is about 3 people
  filter(f_var > 0 & prop_pos > 0.002)

dft_gene_feat_dmet %<>% select(-c(f_var, prop_pos))

# Take this chance to arrange them logically:
# dft_gene_feat_dmet %>%
#   arrange(feature) %>%
#   arrange(desc(str_detect(feature, "_mut$") * 3 + 
#                  str_detect(feature, "_cna$") * 2 + 
#                  str_detect(feature, "_fus$") * 1)
#   )
# Did not work for some reason.

# Back to wide:
dft_gene_feat_dmet %<>%
  pivot_wider(
    names_from = "feature",
    values_from = "value",
    values_fill = NA
  )
# Should be all 1's:
# count(dft_gene_feat_dmet, cpt_genie_sample_id, sort = T)
# count(dft_cpt_dmet, cpt_genie_sample_id, sort = T)

dft_gene_feat_dmet <- left_join(
  dft_cpt_dmet,
  dft_gene_feat_dmet,
  by = c("cpt_genie_sample_id")
)

# Now we have to deal with people that have more than one test:
count(dft_gene_feat_dmet, record_id, ca_seq, sort = T)
# OK, we have two people, so it doesn't seem too important here.  
# Let's drop their second tests.
dft_gene_feat_dmet %<>%
  filter(is_first_cpt) 
# Problem solved - verify with most recent count command.



# Could add other covariates here from dft_pt, but just to start:
dft_bl <- dft_pt %>%
  mutate(
    white = case_when(
      is.na(naaccr_ethnicity_code) ~ NA_real_,
      naaccr_race_code_primary %in% "White" ~ 1,
      T ~ 0
    ),
    hispanic = case_when(
      is.na(naaccr_ethnicity_code) ~ NA_real_,
      naaccr_ethnicity_code %in% "Non-Spanish; non-Hispanic" ~ 0,
      T ~ 1
    )
  ) %>%
  select(record_id, white, hispanic, birth_year)
      

dft_bl <- dft_ca_ind %>%
  select(
    record_id, 
    ca_seq, # works because we've already selected one row per person.
    bca_subtype,
    age_dx, 
    stage_dx_iv,
    dmets_stage_i_iii,
    dx_to_dmets_yrs,
    os_dx_status, 
    tt_os_dx_yrs,
    pfs_i_and_m_adv_status,
    tt_pfs_i_and_m_adv_yrs
  ) %>%
  full_join(., dft_bl, by = "record_id") 

# Based on Slack communication with Evan and team on June 7:
vec_new_bca_levs <- c("HR+/HER2-",
                      "HER2+",
                      "TNBC",
                      "NR_or_NC") # I insist.

dft_bl %<>%
  mutate(
    bca_subtype_manu = case_when(
      is.na(bca_subtype) ~ vec_new_bca_levs[4],
      bca_subtype %in% "HER2-, HR+" ~ vec_new_bca_levs[1],
      bca_subtype %in% c("HER2+, HR-", "HER2+, HR+") ~ vec_new_bca_levs[2],
      bca_subtype %in% c("Triple Negative") ~ vec_new_bca_levs[3],
    ),
    bca_subtype_manu = factor(bca_subtype_manu, levels = vec_new_bca_levs)
  )
      
dft_bl %<>% 
  mutate(
    dx_to_dmet_yrs = if_else(stage_dx_iv %in% "Stage IV", 0, dx_to_dmets_yrs),
    tt_os_dmet_yrs = case_when(
      is.na(dx_to_dmets_yrs) ~ NA_real_,
      T ~ tt_os_dx_yrs - dx_to_dmets_yrs,
    )
    # pfs is already relative to stage IV or dmet date.
  ) 

dft_dmet_surv <- left_join(
  dft_gene_feat_dmet,
  dft_bl,
  by = c("record_id", "ca_seq"),
  multiple = "error"
)

dft_dmet_surv <- dft_dmet_surv %>%
  mutate(
    tt_cpt_dmet_yrs = case_when(
      is.na(dx_to_dmets_yrs) ~ NA_real_,
      T ~ dx_cpt_rep_yrs - dx_to_dmets_yrs,
    ),
    # glmnet won't take negative times:
    tt_cpt_dmet_yrs_pos = if_else(tt_cpt_dmet_yrs < 0, 0, tt_cpt_dmet_yrs),
    stage_dx_iv_num = if_else(stage_dx_iv %in% "Stage IV", 1, 0),
    age_dx_c = age_dx - 40, # approximately centered.
    birth_year_c = birth_year - 1970, # approximately centered.
  )

# Limitation of the method:
dft_dmet_surv %<>%
  filter(!(tt_cpt_dmet_yrs > tt_os_dmet_yrs))
  





x_dmet_os <- dft_dmet_surv %>%
  # just to get the output in readable order we do an alphabet sort first:
  select(order(colnames(dft_dmet_surv))) %>%
  select(
    matches("_mut$"),
    matches("_cna$"),
    matches("_fus$"),
    age_dx_c,
    stage_dx_iv_num,
    birth_year_c,
    white,
    hispanic
  ) %>%
  as.matrix(.)

n_boots <- 300
set.seed(389) # for drawing each boot seed below
dft_boot_dmet_os <- tibble(boot_ind = 1:n_boots) %>%
  mutate(boot_seed = sample.int(n = 10^7, size = n()))

# To force an error:
# dft_boot_dmet_os[2, "boot_seed"] <- 5516804

lasso_wrap_os <- function(bs) {
  out <- tryCatch(
    expr = {
      fit_boot_cv_ltrc_lasso(
      x_mat = x_dmet_os,
      y_df = dft_dmet_surv,
      y_t = "tt_cpt_dmet_yrs_pos",
      y_t2 = "tt_os_dmet_yrs",
      y_e = "os_dx_status",
      boot_seed = bs,
    )
    },
    error=function(cond) {
      message(paste0("Encountered an error with boot seed: ", bs))
      return(NA)
    }
  )
  return(out)
}

# lasso_wrap_os(bs = 29380)
# boot_seed 5516804 caused an error which reproduces.
# lasso_wrap_os(bs = 5516804)

cli::cli_alert_success(paste0("Starting bootstraps at ", Sys.time()))
dft_boot_dmet_os <- dft_boot_dmet_os %>%
  mutate(
    fit = purrr::map(
      .x = boot_seed,
      .f = lasso_wrap_os
    )
  )
cli::cli_alert_success(paste0("Finished bootstraps at ", Sys.time()))

readr::write_rds(
  x = dft_boot_dmet_os,
  file = here('analysis', 'explore', 'lasso_fits_dmet_os.rds')
)

dft_boot_dmet_os <- dft_boot_dmet_os %>%
  filter(!is.na(fit)) %>%
  mutate(
    coef_mat = purrr::map(
      .x = fit, 
      .f = get_cv_lasso_coefs
    ) ,
    n_features_select = purrr::map_dbl(
      .x = coef_mat,
      .f = (function(x) x %>% filter(abs(log_hr) > 0.01) %>% nrow)
    )
  )

dft_coef_dmet_os <- dft_boot_dmet_os %>%
  select(-fit) %>%
  unnest(coef_mat)

dft_coef_dmet_os %<>%
  mutate(feature = forcats::fct_inorder(feature)) %>%
  group_by(feature) %>%
  summarize(
    reliability = mean(abs(log_hr) > 0.01),
    hr = exp(mean(log_hr))
  ) 

gg_coef_flat_dmet_os <- plot_coef_grid_flat(
  dft_coef_dmet_os,
  plot_title = "Model features - OS from dmet",
  ncol = 6
)

gg_coef_reliability_dmet_os <- plot_coef_grid_reliability(
  dft_coef_dmet_os,
  plot_title = "Selected features - OS from dmet",
  ncol = 6
)

gg_hr_forest_dmet_os <- dft_coef_dmet_os %>%
  filter(reliability >= 0.5) %>%
  mutate(
    feature = forcats::fct_drop(feature),
    feature = forcats::fct_rev(feature)) %>% 
  plot_hr_forest(
    .,
    pt_color_col = "reliability",
    pt_shape = 18,
    plot_title = "Frequently selected features for OS from metastasis"
  )

gg_hr_scatter_dmet_os <- dft_coef_dmet_os %>%
  filter(reliability >= 0.5) %>%
  plot_hr_rel_scatter(
    plot_title = "Frequently selected features for OS from metastasis"
  )

ggsave(
  filename = here('output', 'fig', 'features_dmet_os.pdf'),
  plot = gg_coef_flat_dmet_os,
  height = 4, width = 6
)

ggsave(
  filename = here('output', 'fig', 'features_selected_dmet_os.pdf'),
  plot = gg_coef_reliability_dmet_os,
  height = 4, width = 6
)

ggsave(
  filename = here('output', 'fig', 'hr_forest_dmet_os.pdf'),
  plot = gg_hr_forest_dmet_os,
  height = 4, width = 6
)

ggsave(
  filename = here('output', 'fig', 'hr_scatter_dmet_os.pdf'),
  plot = gg_hr_scatter_dmet_os,
  height = 4, width = 6
)





    