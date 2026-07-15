# Description: Fit the predictors of survival from distant metastasis.
# Additionally, fit these for hormone receptor subtypes.

n_boot <- 300
boot_draw_seed <- 102039

library(purrr); library(fs); library(here);
purrr::walk(.x = fs::dir_ls('R'), .f = source)

read_folder <- here('data', 'survival', 'v2', 'prepared_data')
# create the folder if it does not exist:
fs::dir_create(here('data', 'survival', 'v2', 'fit_outputs'))
write_folder <- here('data', 'survival', 'v2', 'fit_outputs')

future::plan(strategy = multisession, workers = 6) 

tic()

dft_dmet_surv_all <- readr::read_rds(
  file = here(read_folder, 'surv_dmet_all.rds')
)

# in order to include bca_subtype_simple_f as a covariate we will need dummies 
#  for it:
dft_dmet_surv_all <- dft_dmet_surv_all %>%
  # make a simpler, standard column name version:
  mutate(
    bca = case_when(
      bca_subtype_f_simple %in% "HR+, HER2-" ~ "hr_pos_her2_neg",
      bca_subtype_f_simple %in% "HER2+" ~ "her2_pos",
      bca_subtype_f_simple %in% "Triple Negative" ~ "trip_neg",
      bca_subtype_f_simple %in% "(NC or NR)" ~ "nc_nr",
      T ~ "error"
    )
  ) %>%
  dummy_cols(., select_columns = "bca") %>% 
  select(-bca, -bca_hr_pos_her2_neg) 


# Load the subset datasets also:
dft_dmet_surv_trip_neg <- readr::read_rds(
  file = here(read_folder, 'surv_dmet_trip_neg.rds')
)
dft_dmet_surv_hr_pos_her2_neg <- readr::read_rds(
  file = here(read_folder, 'surv_dmet_hr_pos_her2_neg.rds')
)
dft_dmet_surv_her2_pos <- readr::read_rds(
  file = here(read_folder, 'surv_dmet_her2_pos.rds')
)



# These are the default adjustments - but I like being explicit.
vec_dmet_surv_confounders <- c(
  "age_dx_c", 
  "stage_dx_iv_num",
  "birth_year_c",
  'asian', 'black', 'race_unk_oth',
  "hispanic",
  'institution_DFCI', 'institution_VICC'
)

boot_models_dmet_all <- surv_fit_dmet_wrap(
  dat = dft_dmet_surv_all,
  boot_rep = n_boot, 
  main_seed = boot_draw_seed,
  additional_features = c(
    "bca_her2_pos", "bca_trip_neg", "bca_nc_nr",
    vec_dmet_surv_confounders
  )
  # therefore, reference group is HR+/HER2-
) 

boot_models_dmet_trip_neg <- surv_fit_dmet_wrap(
  dat = dft_dmet_surv_trip_neg,
  boot_rep = n_boot, 
  main_seed = boot_draw_seed,
  additional_features = vec_dmet_surv_confounders
) 

boot_models_dmet_hr_pos_her2_neg <- surv_fit_dmet_wrap(
  dat = dft_dmet_surv_hr_pos_her2_neg,
  boot_rep = n_boot, 
  main_seed = boot_draw_seed,
  additional_features = vec_dmet_surv_confounders
) 

boot_models_dmet_her2_pos <- surv_fit_dmet_wrap(
  dat = dft_dmet_surv_her2_pos,
  boot_rep = n_boot, 
  main_seed = boot_draw_seed,
  additional_features = vec_dmet_surv_confounders
) 




# Save all the fitted models as RDS:
readr::write_rds(
  x = boot_models_dmet_all,
  file = here(write_folder, 'fit_dmet_all.rds')
)
readr::write_rds(
  x = boot_models_dmet_trip_neg,
  file = here(write_folder, 'fit_dmet_trip_neg.rds')
)
readr::write_rds(
  x = boot_models_dmet_hr_pos_her2_neg,
  file = here(write_folder, 'fit_dmet_hr_pos_her2_neg.rds')
)
readr::write_rds(
  x = boot_models_dmet_her2_pos,
  file = here(write_folder, 'fit_dmet_her2_pos.rds')
)








#####################################
# Repeat for confounder-free models #
#####################################

boot_models_dmet_all_no_conf <- surv_fit_dmet_wrap(
  dat = dft_dmet_surv_all,
  boot_rep = n_boot, 
  main_seed = boot_draw_seed,
  additional_features = c(
    "bca_her2_pos", "bca_trip_neg", "bca_nc_nr"
  )
) 

boot_models_dmet_trip_neg_no_conf <- surv_fit_dmet_wrap(
  dat = dft_dmet_surv_trip_neg,
  boot_rep = n_boot, 
  main_seed = boot_draw_seed,
  additional_features = character(0)
) 

boot_models_dmet_hr_pos_her2_neg_no_conf <- surv_fit_dmet_wrap(
  dat = dft_dmet_surv_hr_pos_her2_neg,
  boot_rep = n_boot, 
  main_seed = boot_draw_seed,
  additional_features = character(0)
) 

boot_models_dmet_her2_pos_no_conf <- surv_fit_dmet_wrap(
  dat = dft_dmet_surv_her2_pos,
  boot_rep = n_boot, 
  main_seed = boot_draw_seed,
  additional_features = character(0)
) 


vec_time_elapsed <- toc()
cli::cli_progress_message(
  paste0(
    vec_time_elapsed$callback_msg,
    " to run dmet boots with ",
    n_boot,
    " repetitions."
  )
)

# Save all the fitted models as RDS:
readr::write_rds(
  x = boot_models_dmet_all_no_conf,
  file = here(write_folder, 'fit_dmet_all_no_conf.rds')
)
readr::write_rds(
  x = boot_models_dmet_trip_neg_no_conf,
  file = here(write_folder, 'fit_dmet_trip_neg_no_conf.rds')
)
readr::write_rds(
  x = boot_models_dmet_hr_pos_her2_neg_no_conf,
  file = here(write_folder, 'fit_dmet_hr_pos_her2_neg_no_conf.rds')
)
readr::write_rds(
  x = boot_models_dmet_her2_pos_no_conf,
  file = here(write_folder, 'fit_dmet_her2_pos_no_conf.rds')
)


vec_time_elapsed <- toc()
cli::cli_alert_success(
  paste0(
    vec_time_elapsed$callback_msg,
    " to run dmet boots (+ no confounder models) with ",
    n_boot,
    " repetitions."
  )
)