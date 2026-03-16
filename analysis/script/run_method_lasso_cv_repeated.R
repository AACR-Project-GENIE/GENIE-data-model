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

tic()




# sim_n80 %<>% slice(1:2)
sim_n500 %<>% slice(1:100)

# sim_n80 %<>%
#   mutate(
#     analysis_method = "method_lasso_cv_repeat",
#     coef_est = furrr::future_map2(
#       .x = gen_dat,
#       .y = sim_seed,
#       .f = (function(d, s) {
#         method_lasso_cv_repeat(
#           test_dat,
#           main_seed = s * 7,
#           rep = 25, # low number - but it will do for testing
#         )
#       }),
#       .progress = T
#     )
#   )

# readr::write_rds(
#   x = sim_n80,
#   file = here('sim', 'run_methods', 'gen_dat_one_n80_lasso_cv_repeated.rds')
# )

sim_n500 %<>%
  mutate(
    analysis_method = "method_lasso_cv_repeat",
    coef_est = furrr::future_map2(
      .x = gen_dat,
      .y = sim_seed,
      .f = (function(d, s) {
        method_lasso_cv_repeat(
          test_dat,
          main_seed = s * 7, 
          rep = 25, # low number - but it will do for testing
        )
      }),
      .progress = T
    )
  )

readr::write_rds(
  x = sim_n500,
  file = here('sim', 'run_methods', 'gen_dat_one_n500_lasso_cv_repeated_f100.rds')
)

cli::cli_alert_success(
  glue(
    "Ran the CV repeat method in {toc()$callback_msg}."
  )
)





# # Example of running one dataset:
# test_dat <- sim_n500 %>%
#   slice(1) %>%
#   pull(gen_dat) %>%
#   `[[`(1)
# method_lasso_cv_repeat(
#   test_dat,
#   main_seed = 2019,
#   rep = 25, # low number - but it will do for testing
# )
# toc()

# method_univar_cox(test_dat)