# Description: Fit the predictors of survival from the start
#   of select drug classes

n_boot <- 300
boot_draw_seed <- 102039

library(purrr); library(fs); library(here);
purrr::walk(.x = fs::dir_ls('R'), .f = source)

read_folder <- here('data', 'survival', 'drug', 'prepared_data')
# create the folder if it does not exist:
write_folder <- here('data', 'survival', 'drug', 'fit_outputs')

future::plan(strategy = multisession, workers = 6) 

tic()

dft_drug_surv_nest <- readr::read_rds(
  file = here(read_folder, 'drug_surv_nest.rds')
) 

# Arbitrary decision to filter drug classes with less than 30 
#   cases.
dft_drug_surv_nest %<>%
  mutate(
    nrow_surv = purrr::map_dbl(
      .x = dat_surv,
      .f = nrow
    )
  ) %>%
  filter(nrow_surv >= 30) %>%
  select(-nrow_surv)


# For PFS we are taking a bit of a hack for now:  We can use the
#   precomuputed (regimen-level) variables for everyone except those who
#   had a progression event after starting the regimen but before starting
#   the particular drug in the regimen we want to study.
# At the time of this writing this affects a single case in two drug classes,
#   which seems like an acceptable starting point.
dft_drug_surv_nest %<>%
  mutate(
    dat_surv_pfs = purrr::map(
      .x = dat_surv,
      .f = (function(x) filter(x, tt_pfs_i_and_m_drug_start_yrs >= 0))
    )
  )

# Adjustment variables:
vec_drug_surv_confounders <- c(
  "age_drug_start", 
  "stage_dx_iv_num",
  "birth_year_c",
  "white",
  "hispanic",
  "bca_her2_pos",
  "bca_nc_nr",
  "bca_trip_neg"
)

# Fit the overall survival model with confounders:
dft_drug_surv_nest %<>%
  mutate(
    boots_os = purrr::map(
      .x = dat_surv,
      .f = (function(x) {
        surv_fit_dmet_wrap_no_lt(
          dat = x,
          boot_rep = n_boot, 
          main_seed = boot_draw_seed,
          additional_features = vec_drug_surv_confounders,
          outcome_var = "tt_os_drug_start_yrs",
          outcome_ind = "os_g_status"
        ) 
      })
    )
  )

# Same model, except without confounders (sensitivity):
dft_drug_surv_nest %<>%
  mutate(
    boots_os_no_conf = purrr::map(
      .x = dat_surv,
      .f = (function(x) {
        surv_fit_dmet_wrap_no_lt(
          dat = x,
          boot_rep = n_boot, 
          main_seed = boot_draw_seed,
          additional_features = character(0),
          outcome_var = "tt_os_drug_start_yrs",
          outcome_ind = "os_g_status"
        ) 
      })
    )
  )

# Fit the PFS model with confounders:
dft_drug_surv_nest %<>%
  mutate(
    boots_pfs = purrr::map(
      .x = dat_surv_pfs,
      .f = (function(x) {
        surv_fit_dmet_wrap_no_lt(
          dat = x,
          boot_rep = n_boot, 
          main_seed = boot_draw_seed,
          additional_features = vec_drug_surv_confounders,
          outcome_var = "tt_pfs_i_and_m_drug_start_yrs",
          outcome_ind = "pfs_i_and_m_g_status"
        ) 
      })
    )
  )

# Same model, except without confounders (sensitivity):
dft_drug_surv_nest %<>%
  mutate(
    boots_pfs_no_conf = purrr::map(
      .x = dat_surv_pfs,
      .f = (function(x) {
        surv_fit_dmet_wrap_no_lt(
          dat = x,
          boot_rep = n_boot, 
          main_seed = boot_draw_seed,
          additional_features = character(0),
          outcome_var = "tt_pfs_i_and_m_drug_start_yrs",
          outcome_ind = "pfs_i_and_m_g_status"
        ) 
      })
    )
  )



toc()


# Save all the fitted models as RDS:
readr::write_rds(
  x = dft_drug_surv_nest,
  file = here(write_folder, 'fit_dmet_drug_nest.rds')
)



# Useful if you want to pull one out for looking:
# dft_surv_test <- dft_drug_surv_nest %>% slice(1) %>% pull(dat_surv) %>% `[[`(.,1)
