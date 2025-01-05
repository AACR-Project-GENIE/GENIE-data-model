# Description: Runs the univariate cox method on simulated datasets.

library(fs)
library(here)
library(purrr)

purrr::walk(.x = fs::dir_ls('R'), .f = source)

sim_n80 <- readr::read_rds(
  here('sim', 'run_methods', 'gen_dat_one_n80_cox_uni.rds')
)

sim_n500 <- readr::read_rds(
  here('sim', 'run_methods', 'gen_dat_one_n500_cox_uni.rds')
)

# Example of running one time:
# test_beta <- sim_n80 %>% slice(2) %>% pull(beta_valid) %>% unlist(.)
# test_coef_dat <- sim_n80 %>% slice(2) %>% pull(coef_est) %>% `[[`(.,1)
# test_gen_dat <- sim_n80 %>% slice(2) %>% pull(gen_dat_valid) %>% `[[`(.,1)


eval_wrapper_cox <- function(sim_data) {
  # Add auc:
  sim_data %<>%
    mutate(
      auc = purrr::map2_dbl(
        .x = beta_valid,
        .y = coef_est,
        .f = (function(b, c) {
          eval_beta_auc_pval(
            true_beta = b,
            coef_dat = c,
            return_type = "auc"
          )
        })
      )
    )
  
  # Add sensitivity at the traditional p = 0.05
  sim_data %<>%
    mutate(
      spec_thresh = "p=0.05",
      spec_at_thresh = purrr::map2_dbl(
        .x = beta_valid,
        .y = coef_est,
        .f = (function(b, c) {
          eval_spec_at_thresh(
            true_beta = b,
            coef_dat = c,
            thresh_param = "p.value",
            thresh_to_test = 0.05,
            low_thresh_good = T
          )
        })
      )
    )
  
  # Add specificity at the traditional p = 0.05
  sim_data %<>%
    mutate(
      sens_thresh = "p=0.05",
      sens_at_thresh = purrr::map2_dbl(
        .x = beta_valid,
        .y = coef_est,
        .f = (function(b, c) {
          eval_sens_at_thresh(
            true_beta = b,
            coef_dat = c,
            thresh_param = "p.value",
            thresh_to_test = 0.05,
            low_thresh_good = T
          )
        })
      )
    )
  
  
  # add several bias metrics in:
  sim_data %<>%
    mutate(
      bias_dat = purrr::map2(
        .x = beta_valid,
        .y = coef_est,
        .f = (function(b, c) {
          eval_beta_bias(
            true_beta_valid = b,
            coef_dat = c
          )
        })
      )
    ) %>%
    unnest(bias_dat)
  
  sim_data %<>%
    mutate(
      coef_2x2_dat = purrr::map2(
        .x = beta_valid,
        .y = coef_est,
        .f = (function(b, c) {
          eval_coef_2x2_cox(
            true_beta_valid = b,
            coef_dat = c
          )
        })
      )
    ) %>%
    unnest(coef_2x2_dat)
  
  return(sim_data)
  
}
  
# Where the actual work is done:
sim_n80 %<>% eval_wrapper_cox(.)
sim_n500 %<>% eval_wrapper_cox(.)
    



readr::write_rds(
  x = sim_n80,
  file = here('sim', 'evaled_methods', 'gen_dat_one_n80_cox_uni.rds')
)


readr::write_rds(
  x = sim_n500,
  file = here('sim', 'evaled_methods', 'gen_dat_one_n500_cox_uni.rds')
)



