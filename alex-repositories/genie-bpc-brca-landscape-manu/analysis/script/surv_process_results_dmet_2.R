# Description: Process the raw model fits into results.
# Author: Alex Paynter



library(fs); library(purrr); library(here);
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)


raw_folder <- here("data", "survival", 'v2', 'fit_outputs')
output_folder <- here('data', 'survival', 'v2', 'fit_outputs', 'fit_summary')


boot_models_dmet_all <- readr::read_rds(
  here(raw_folder, "fit_dmet_all.rds")
)
boot_models_dmet_trip_neg <- readr::read_rds(
  here(raw_folder, "fit_dmet_trip_neg.rds")
)
boot_models_dmet_hr_pos_her2_neg <- readr::read_rds(
  here(raw_folder, "fit_dmet_hr_pos_her2_neg.rds")
)
boot_models_dmet_her2_pos <- readr::read_rds(
  here(raw_folder, "fit_dmet_her2_pos.rds")
)

# No confounder models need to be processed too:
boot_models_dmet_all_no_conf <- readr::read_rds(
  here(raw_folder, "fit_dmet_all_no_conf.rds")
)
boot_models_dmet_trip_neg_no_conf <- readr::read_rds(
  here(raw_folder, "fit_dmet_trip_neg_no_conf.rds")
)
boot_models_dmet_hr_pos_her2_neg_no_conf <- readr::read_rds(
  here(raw_folder, "fit_dmet_hr_pos_her2_neg_no_conf.rds")
)
boot_models_dmet_her2_pos_no_conf <- readr::read_rds(
  here(raw_folder, "fit_dmet_her2_pos_no_conf.rds")
)





resample_dmet_wrap <- function(dat) {
  resample_coef_helper(
    dat = dat,
    exp_coefs = F,
    estimate_col_rename = "log_hr" 
  )
}

dft_coef_dmet_os_all <- resample_dmet_wrap(
  dat = boot_models_dmet_all
)
dft_coef_dmet_os_trip_neg <- resample_dmet_wrap(
  dat = boot_models_dmet_trip_neg
)
dft_coef_dmet_os_hr_pos_her2_neg <- resample_dmet_wrap(
  dat = boot_models_dmet_hr_pos_her2_neg
)
dft_coef_dmet_os_her2_pos <- resample_dmet_wrap(
  dat = boot_models_dmet_her2_pos
)

dft_coef_dmet_os_all_no_conf <- resample_dmet_wrap(
  dat = boot_models_dmet_all_no_conf
)
dft_coef_dmet_os_trip_neg_no_conf <- resample_dmet_wrap(
  dat = boot_models_dmet_trip_neg_no_conf
)
dft_coef_dmet_os_hr_pos_her2_neg_no_conf <- resample_dmet_wrap(
  dat = boot_models_dmet_hr_pos_her2_neg_no_conf
)
dft_coef_dmet_os_her2_pos_no_conf <- resample_dmet_wrap(
  dat = boot_models_dmet_her2_pos_no_conf
)



# Rename some of the coefficients (collaborator preference):
dft_coef_dmet_os_all %<>% 
  mutate(term = manu_term_rename_helper(term))
dft_coef_dmet_os_all_no_conf %<>% 
  mutate(term = manu_term_rename_helper(term))

dft_coef_dmet_os_her2_pos %<>% 
  mutate(term = manu_term_rename_helper(term))
dft_coef_dmet_os_her2_pos_no_conf %<>% 
  mutate(term = manu_term_rename_helper(term))

dft_coef_dmet_os_hr_pos_her2_neg %<>% 
  mutate(term = manu_term_rename_helper(term))
dft_coef_dmet_os_hr_pos_her2_neg_no_conf %<>% 
  mutate(term = manu_term_rename_helper(term))

dft_coef_dmet_os_trip_neg %<>% 
  mutate(term = manu_term_rename_helper(term))
dft_coef_dmet_os_trip_neg_no_conf %<>% 
  mutate(term = manu_term_rename_helper(term))









# Write the outputs:
write_wrap_surv_sum <- function(obj, name) {
    readr::write_rds(
      x = obj,
      file = here(output_folder, paste0(name, ".rds"))
    )
}

write_wrap_surv_sum(
  dft_coef_dmet_os_all,
     "coef_dmet_os_all"
)
write_wrap_surv_sum(
  dft_coef_dmet_os_trip_neg,
     "coef_dmet_os_trip_neg"
)
write_wrap_surv_sum(
  dft_coef_dmet_os_hr_pos_her2_neg,
  "coef_dmet_os_hr_pos_her2_neg"
)
write_wrap_surv_sum(
  dft_coef_dmet_os_her2_pos,
  "coef_dmet_os_her2_pos"
)
write_wrap_surv_sum(
  dft_coef_dmet_os_all_no_conf,
  "coef_dmet_os_all_no_conf"
)
write_wrap_surv_sum(
  dft_coef_dmet_os_trip_neg_no_conf,
  "coef_dmet_os_trip_neg_no_conf"
)
write_wrap_surv_sum(
  dft_coef_dmet_os_hr_pos_her2_neg_no_conf,
  "coef_dmet_os_hr_pos_her2_neg_no_conf"
)
write_wrap_surv_sum(
  dft_coef_dmet_os_her2_pos_no_conf,
  "coef_dmet_os_her2_pos_no_conf"
)



####
# Comparisons between the confounder and no confounder models

model_compare_os_all <- model_compare_boot_lasso(
  dft_coef_dmet_os_all,
  dft_coef_dmet_os_all_no_conf
)
model_compare_os_trip_neg <- model_compare_boot_lasso(
  dft_coef_dmet_os_trip_neg,
  dft_coef_dmet_os_trip_neg_no_conf
)
model_compare_os_her2_pos <- model_compare_boot_lasso(
  dft_coef_dmet_os_her2_pos,
  dft_coef_dmet_os_her2_pos_no_conf
)
model_compare_os_hr_pos_her2_neg <- model_compare_boot_lasso(
  dft_coef_dmet_os_hr_pos_her2_neg,
  dft_coef_dmet_os_hr_pos_her2_neg_no_conf
)

write_wrap_surv_sum(
  model_compare_os_all,
  "model_compare_os_all"
)
write_wrap_surv_sum(
  model_compare_os_trip_neg,
  "model_compare_os_trip_neg"
)
write_wrap_surv_sum(
  model_compare_os_her2_pos,
  "model_compare_os_her2_pos"
)
write_wrap_surv_sum(
  model_compare_os_hr_pos_her2_neg,
  "model_compare_os_hr_pos_her2_neg"
)



