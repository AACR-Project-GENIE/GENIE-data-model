# Description: Runs the univariate cox method on simulated datasets.

library(fs)
library(here)
library(purrr)

purrr::walk(.x = fs::dir_ls('R'), .f = source)

sim_n80 <- readr::read_rds(
  here('sim', 'run_methods',
       'gen_dat_one_n80_lasso_5fcv.rds')
)

sim_n500 <- readr::read_rds(
  here('sim', 'run_methods', 
       'gen_dat_one_n500_lasso_5fcv.rds')
)

sim_n80 %<>% unnest(fit)
sim_n500 %<>% unnest(fit)


fix_coef_dat <- function(dat) {
  # If here because I may fix this later on:
  if ("log_hr" %in% names(dat)) {
    dat %<>% rename(estimate = log_hr)
  }
  dat %>%
    mutate(abs_estimate = abs(estimate))
}

fix_coef_dat_apply <- function(sim_data) {
  sim_data %<>% mutate(
    coef_est = purrr::map(
      .x = coef_est,
      .f = (function(f) fix_coef_dat(f)) 
    )
  )
}

sim_n80 %<>% fix_coef_dat_apply(.)
sim_n500 %<>% fix_coef_dat_apply(.)
  



# 
# test_beta <- sim_n500 %>% slice(1) %>% pull(beta_valid) %>% `[[`(.,1)
# test_coef_dat <- sim_n500 %>% slice(1) %>% pull(coef_est) %>% `[[`(.,1)
# eval_beta_auc_lambda(test_beta, test_dat, return_type = "plot")

eval_sens_at_thresh(
  test_beta,
  test_coef_dat,
  thresh_param = "abs_estimate",
  thresh_to_test = 0.0001,
  low_thresh_good = F
)

eval_spec_at_thresh(
  test_beta,
  test_coef_dat,
  thresh_param = "abs_estimate",
  thresh_to_test = 0.0001,
  low_thresh_good = F
)

###########################
# Add in AUC calculation: #
###########################


eval_wrap_lasso_5fcv <- function(sim_data) {
  sim_data %<>%
    mutate(
      auc = purrr::map2_dbl(
        .x = beta_valid,
        .y = dropout_dat,
        .f = (function(b, d) {
          eval_beta_auc_lambda(
            true_beta = b,
            dropout_dat = d,
            return_type = "auc"
          )
        })
      )
    )
  
  # This one is a bit odd:  we use the estimate
  # itself as the threshold for sens/spec. This is because
  # coef_est is already AT the threshold we want (lambda.min)
  sim_data %<>%
    mutate(
      spec_thresh = "lambda.min",
      spec_at_thresh = purrr::map2_dbl(
        .x = beta_valid,
        .y = coef_est,
        .f = (function(b, d) {
          eval_spec_at_thresh(
            true_beta = b,
            coef_dat = d,
            thresh_param = "abs_estimate",
            thresh_to_test = 0.0001,
            low_thresh_good = F
          )
        })
      )
    )
  
  sim_data %<>%
    mutate(
      sens_at_thresh = purrr::map2_dbl(
        .x = beta_valid,
        .y = coef_est,
        .f = (function(b, d) {
          eval_sens_at_thresh(
            true_beta = b,
            coef_dat = d,
            thresh_param = "abs_estimate",
            thresh_to_test = 0.0001,
            low_thresh_good = F
          )
        })
      )
    )
  
  
  
  sim_data %<>%
    mutate(
      bias_dat = purrr::map2(
        .x = beta_valid,
        .y = coef_est,
        .f = (function(b, c) {
          # I didn't save the names right on the first run:
          if ("log_hr" %in% names(c)) {
            c %<>% rename(estimate = log_hr)
          }
          eval_beta_bias(
            true_beta_valid = b,
            coef_dat = c
          )
        })
      )
    ) %>%
    unnest(bias_dat)
}
  
sim_n80 %<>% eval_wrap_lasso_5fcv(.)
sim_n500 %<>% eval_wrap_lasso_5fcv(.)


readr::write_rds(
  x = sim_n80,
  file = here('sim', 'evaled_methods', 'gen_dat_one_n80_lasso_5fcv.rds')
)

readr::write_rds(
  x = sim_n500,
  file = here('sim', 'evaled_methods', 'gen_dat_one_n500_lasso_5fcv.rds')
)


