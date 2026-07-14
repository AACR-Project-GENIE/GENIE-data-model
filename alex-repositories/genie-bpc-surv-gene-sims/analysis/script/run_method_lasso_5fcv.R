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




# Run once:
# test_data <- sim_n80 %>% slice(1) %>% pull(gen_dat_valid) %>% `[[`(.,1)
# bleh <- method_lasso_cv_once(dat = test_data, seed = 2)  


sim_n80 %<>%
  mutate(
    analysis_method = "method_lasso_5fcv",
    fit = furrr::future_map2(
      .x = gen_dat_valid,
      .y = sim_seed,
      .f = (function(d, s) {
        method_lasso_cv_once(
          dat = d,
          seed = s * 13,
          cv_folds = 5,
          lambda_for_coef_est = "lambda.min"
        )
      }),
      .progress = T
    )
  )


readr::write_rds(
  x = sim_n80,
  file = here(
    'sim', 'run_methods', 
    'gen_dat_one_n80_lasso_5fcv.rds'
  ) 
)




sim_n500 %<>%
  mutate(
    analysis_method = "method_lasso_5fcv",
    fit = furrr::future_map2(
      .x = gen_dat_valid,
      .y = sim_seed,
      .f = (function(d, s) {
        method_lasso_cv_once(
          dat = d,
          seed = s * 13,
          cv_folds = 5,
          lambda_for_coef_est = "lambda.min"
        )
      }),
      .progress = T
    )
  )
readr::write_rds(
  x = sim_n500,
  file = here(
    'sim', 'run_methods', 
    'gen_dat_one_n500_lasso_5fcv.rds'
  )
)

cli::cli_alert_success(
  glue(
    "Ran the one time lasso (5fcv) method in {toc()$callback_msg}."
  )
)
