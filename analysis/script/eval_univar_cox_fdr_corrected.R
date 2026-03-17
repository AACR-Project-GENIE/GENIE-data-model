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

# add the q values in :
sim_n80 %<>% mutate(coef_est = purrr::map(.x = coef_est, .f = add_qval))
sim_n500 %<>% mutate(coef_est = purrr::map(.x = coef_est, .f = add_qval))

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
            return_type = "auc",
            d_name = 'q.value'
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
            thresh_param = "q.value",
            thresh_to_test = 0.05,
            low_thresh_good = T
          )
        })
      )
    )

  # Add specificity at the traditional p = 0.05
  sim_data %<>%
    mutate(
      sens_thresh = "q=0.05",
      sens_at_thresh = purrr::map2_dbl(
        .x = beta_valid,
        .y = coef_est,
        .f = (function(b, c) {
          eval_sens_at_thresh(
            true_beta = b,
            coef_dat = c,
            thresh_param = "q.value",
            thresh_to_test = 0.05,
            low_thresh_good = T
          )
        })
      )
    )

  sim_data %<>%
    mutate(
      beta_dat = purrr::map2(
        .x = beta_valid,
        .y = coef_est,
        .f = (function(b, c) {
          eval_beta_df_uni_cox(
            beta_valid = b,
            coef_est = c
          )
        })
      )
    )
  # beta_df does not get unnested, it's a processing step.

  sim_data %<>%
    mutate(
      coef_2x2_dat = purrr::map(
        .x = beta_dat,
        .f = (function(b) {
          eval_coef_2x2(
            beta_dat = b
          )
        })
      )
    ) %>%
    unnest(coef_2x2_dat)

  sim_data %<>%
    mutate(
      power = eval_power(tp_beta = tp_beta, fn_beta = fn_beta),
      selectivity = eval_selectivity(tn_beta = tn_beta, fp_beta = fp_beta)
    )

  sim_data %<>%
    mutate(
      avg_abs_bias = purrr::map_dbl(
        .x = beta_dat,
        .f = \(b) eval_avg_bias(b, absolute = T, inclusion = "all")
      ),
      avg_abs_bias_selected = purrr::map_dbl(
        .x = beta_dat,
        .f = \(b) eval_avg_bias(b, absolute = T, inclusion = "selected")
      ),
      # don't really need these, but they can be calculated easily enough:
      avg_bias = purrr::map_dbl(
        .x = beta_dat,
        .f = \(b) eval_avg_bias(b, absolute = F, inclusion = "all")
      ),
      avg_bias_selected = purrr::map_dbl(
        .x = beta_dat,
        .f = \(b) eval_avg_bias(b, absolute = F, inclusion = "selected")
      )
    )

  return(sim_data)
}

# Where the actual work is done:
sim_n80 %<>% eval_wrapper_cox(.)
sim_n500 %<>% eval_wrapper_cox(.)

# sim_n80 %>% summarize(
#   tp = mean(tp_beta, na.rm = T),
#   fp = mean(fp_beta, na.rm = T),
#   tn = mean(tn_beta, na.rm = T),
#   fn = mean(fn_beta, na.rm = T)
# )

readr::write_rds(
  x = sim_n80,
  file = here('sim', 'evaled_methods', 'gen_dat_one_n80_cox_uni.rds')
)


readr::write_rds(
  x = sim_n500,
  file = here('sim', 'evaled_methods', 'gen_dat_one_n500_cox_uni.rds')
)
