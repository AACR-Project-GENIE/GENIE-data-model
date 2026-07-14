# Description: Runs the method on the datasets

library(fs)
library(here)
library(purrr)

purrr::walk(.x = fs::dir_ls('R'), .f = source)

sim_n80 <- readr::read_rds(
  here('sim', 'gen_data', 'gen_dat_one_n80.rds')
)

sim_n500 <- readr::read_rds(
  here('sim', 'gen_data', 'gen_dat_one_n500.rds')
)

future::plan(strategy = multisession, workers = 6) 
# time note:  rep = 25, first 200 rows took about 4.8 hours on 
#   a personal computer running 6 threads for the n=80 + n=500.
# Should be able to run with all 1000 in about a day with the same number of reps.

tic()



# cli::cli_alert_danger("lasso_cv_boot only running the first 200 samples for time - fix when ready")
# sim_n80 %<>% slice(1:200)
# sim_n500 %<>% slice(1:200)

# test_dat <- sim_n500 %>% slice(1) %>% pull(gen_dat_valid) %>% `[[`(.,1) 
# 
# boot_fold_help(test_dat, seed = 23) 

# surv_fit_boot_sim(
#   dat = test_dat,
#   main_seed = 23,
#   rep = 2,
#   ignore_cols = "id_obs"
# )
# 
# fit_boot_cv_ltrc_lasso_2(
#   dat = test_dat,
#   x_col = "x",
#   y_col = "y",
#   event_col = "event",
#   boot_seed = 23,
#   cv_folds = 5
# )


sim_n80 %<>%
  mutate(
    analysis_method = "method_lasso_cv_boot",
    fit = furrr::future_map2(
      .x = gen_dat_valid,
      .y = sim_seed,
      .f = (function(d, s) {
        surv_fit_boot_sim(
          d,
          main_seed = s * 7,
          rep = 25, # low number - but it will do for testing
        )
      }),
      .progress = T
    )
  )


readr::write_rds(
  x = sim_n80,
  file = here('sim', 'run_methods', 'gen_dat_one_n80_lasso_cv_boot.rds')
)

sim_n500 %<>%
  mutate(
    analysis_method = "method_lasso_cv_boot",
    fit = furrr::future_map2(
      .x = gen_dat_valid,
      .y = sim_seed,
      .f = (function(d, s) {
        surv_fit_boot_sim(
          d,
          main_seed = s * 7,
          rep = 25, # low number - but it will do for testing
        )
      }),
      .progress = T
    )
  )

readr::write_rds(
  x = sim_n500,
  file = here('sim', 'run_methods', 'gen_dat_one_n500_lasso_cv_boot.rds')
)

cli::cli_alert_success(
  glue(
    "Ran the CV boot method in {toc()$callback_msg}."
  )
)




