# Description:  This was a request from the breast cancer group as a compliment
#  to Evan's analysis.  I did not build on surv_prep_dmet because we don't 
#  need any of the complexity of genomic data here.

library(here); library(purrr); library(fs)
purrr::walk(.x = fs::dir_ls('R'), .f = source)
library(survminer)

dft_ca_ind <- readr::read_rds(
  here('data', 'clin_data_cohort', 'dft_ca_ind.rds')
)
dft_cpt <- readr::read_rds(
  here('data', 'clin_data_cohort', 'dft_cpt.rds')
)

out_dir <- here('output', 'fig', 'manu')




  



# age_dx is a floored version of ca_cadx_int in this dataset.

lev_age <- c(
  "18-39 at dx.",
  "40-49 at dx.",
  "50-59 at dx."
)

dft_surv_age_bca <- dft_ca_ind %>%
  mutate(
    age_custom = case_when(
      age_dx < 40 & age_dx >= 18 ~ lev_age[1],
      age_dx < 50 & age_dx >= 40 ~ lev_age[2],
      age_dx >= 50 ~ lev_age[3]
    ),
    age_custom = factor(age_custom, levels = lev_age)
  )



dft_surv_age_bca %<>% 
  mutate(
    # in the breast dataset this is fairly simple since stage IV and dmet are
    #   synonymous.
    dx_to_dmets_yrs = if_else(
      stage_dx_iv %in% "Stage IV", 
      0, 
      dx_to_dmets_yrs
    ),
    tt_os_dmet_yrs = case_when(
      is.na(dx_to_dmets_yrs) ~ NA_real_,
      T ~ tt_os_dx_yrs - dx_to_dmets_yrs,
    )
    # pfs is already relative to stage IV or dmet date.
  ) 

dft_surv_age_bca %<>%
  filter(!is.na(tt_os_dmet_yrs) & !is.na(bca_subtype_f_simple)) %>%
  select(
    record_id, ca_seq, 
    age_custom,
    matches("^bca"),
    dx_to_dmets_yrs,
    tt_os_dmet_yrs,
    os_dmet_status = os_dx_status # death is death - no matter what it's from.
  )



dft_first_cpt <- dft_cpt %>%
  group_by(record_id, ca_seq) %>%
  arrange(cpt_number) %>%
  slice(1) %>%
  ungroup(.) %>%
  select(record_id, ca_seq, dx_cpt_rep_yrs)

dft_surv_age_bca %<>%
  left_join(
    ., dft_first_cpt, by = c('record_id', 'ca_seq')
  )
  


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

dft_surv_age_bca %<>% add_specific_dmet_vars(.)

# ggplot(dft_surv_age_bca, aes(x = tt_cpt_dmet_yrs, y = tt_os_dmet_yrs)) + geom_point()

dft_surv_age_bca %<>%
  filter(tt_cpt_dmet_yrs_pos <= tt_os_dmet_yrs)

# We'll save this for later use because we'll need a whole report now:
readr::write_rds(
  dft_surv_age_bca,
  file = here('data', 'survival', 'age', 'surv_age_bca_dat.rds')
)



gg_hr_pos <- plot_surv_age(
  dft_surv_age_bca,
  bca_subgroup = "HR+, HER2-"
)
gg_her2_pos <- plot_surv_age(
  dft_surv_age_bca,  
  bca_subgroup = "HER2+"
)
gg_trip_neg <- plot_surv_age(
  dft_surv_age_bca, 
  bca_subgroup = "Triple Negative"
  )
gg_all <- plot_surv_age(
  dft_surv_age_bca,
  bca_subgroup = NULL
)

# And then a rather inadvisable request to break the HER2+ group into two:
gg_her2_pos_hr_pos <- plot_surv_age(
  dft_surv_age_bca,
  bca_subgroup = "HR+, HER2+",
  bca_subgroup_var = "bca_subtype_f"
)
gg_her2_pos_hr_neg <- plot_surv_age(
  dft_surv_age_bca,
  bca_subgroup = "HR-, HER2+",
  bca_subgroup_var = "bca_subtype_f"
)


surv_age_save_help <- function(gg, file) {
  readr::write_rds(
    x = gg,
    file = here('data', 'survival', 'age', paste0(file, '.rds'))
  )
  
  ggsave(
    plot = gg, height = 5, width = 5,
    filename = here(out_dir, paste0(file, '.pdf'))
  )
  
  return(NULL)
}

surv_age_save_help(gg = gg_hr_pos, file = 'gg_age_surv_hr_pos')
surv_age_save_help(gg_her2_pos, 'gg_age_surv_her2_pos')
surv_age_save_help(gg_trip_neg, 'gg_age_surv_trip_neg')
surv_age_save_help(gg_all, 'gg_age_surv_all')

# saving these differently because they shouldn't be included long term:
surv_age_save_help(gg = gg_her2_pos_hr_pos, 'aside_age_surv_her2_pos_hr_pos')
surv_age_save_help(gg = gg_her2_pos_hr_neg, 'aside_age_surv_her2_pos_hr_neg')




