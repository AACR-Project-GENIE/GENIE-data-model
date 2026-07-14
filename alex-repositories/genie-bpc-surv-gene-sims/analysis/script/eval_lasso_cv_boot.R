# Description: Runs the univariate cox method on simulated datasets.

library(fs)
library(here)
library(purrr)

purrr::walk(.x = fs::dir_ls('R'), .f = source)

sim_n80 <- readr::read_rds(
  here('sim', 'run_methods', 'gen_dat_one_n80_lasso_cv_boot.rds')
)

sim_n500 <- readr::read_rds(
  here('sim', 'run_methods', 'gen_dat_one_n500_lasso_cv_boot.rds')
)

STABILITY_THRESH <- 0.2

# sim_n80 %>% slice(1) %>% pull(coef_est) %>% `[[`(.,1)


# There's an if statement here because eventually I'd like this to be done at the previous step, but I didn't have time to add it previously.
# For now it just takes a few seconds to compute it here, so off we go:
if (!("coef_est" %in% colnames(sim_n80))) {
  sim_n80 %<>%
    mutate(
      coef_est = purrr::map(
        .x = fit,
        .f = (function(f) resample_coef_helper(f))
      )
    )
  
  sim_n500 %<>%
    mutate(
      coef_est = purrr::map(
        .x = fit,
        .f = (function(f) resample_coef_helper(f))
      )
    ) 
  
}


###########################
# Add in AUC calculation: #
###########################

eval_wrap_lasso_cv_boot <- function(sim_data) {
  sim_data %<>%
    mutate(
      auc = purrr::map2_dbl(
        .x = beta_valid,
        .y = coef_est,
        .f = (function(b, c) {
          eval_beta_auc_stability(
            true_beta = b,
            coef_dat = c,
            return_type = "auc"
          )
        })
      )
    )
  
  # sim_data %<>%
  #   # add several bias metrics in:
  #   mutate(
  #     bias_dat = purrr::map2(
  #       .x = beta_valid,
  #       .y = coef_est,
  #       .f = (function(b, c) {
  #         eval_beta_bias(
  #           true_beta_valid = b,
  #           coef_dat = c
  #         )
  #       })
  #     )
  #   ) %>%
  #   unnest(bias_dat)
  
  sim_data %<>%
    mutate(
      spec_thresh = paste0("stability=",STABILITY_THRESH),
      spec_at_thresh = purrr::map2_dbl(
        .x = beta_valid,
        .y = coef_est,
        .f = (function(b, d) {
          eval_spec_at_thresh(
            true_beta = b,
            coef_dat = d,
            thresh_param = "stability",
            thresh_to_test = STABILITY_THRESH,
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
            thresh_param = "stability",
            thresh_to_test = STABILITY_THRESH,
            low_thresh_good = F
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
          eval_beta_df_lasso_boot(
            beta_valid = b,
            coef_est = c,
            select_thresh = STABILITY_THRESH
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



sim_n80 %<>% eval_wrap_lasso_cv_boot(.)
sim_n500 %<>% eval_wrap_lasso_cv_boot(.)



readr::write_rds(
  x = sim_n80,
  file = here('sim', 'evaled_methods', 'gen_dat_one_n80_lasso_cv_boot.rds')
)

readr::write_rds(
  x = sim_n500,
  file = here('sim', 'evaled_methods', 'gen_dat_one_n500_lasso_cv_boot.rds')
)


# A good test case on bias:
# test_beta <- sim_n500 %>% slice(1) %>% pull(beta_valid) %>% `[[`(.,1)
# test_beta <- test_beta[abs(test_beta) > 0.0000001] 
# test_coef_est <- sim_n500 %>% slice(1) %>% pull(coef_est) %>% `[[`(.,1)
# test_coef_est %>%
#   filter(term %in% names(test_beta)) %>%
#   mutate(truth = test_beta,
#          diff = estimate - truth) %>%
#   summarize(
#     abs_bias = mean(abs(diff)),
#     bias = mean(diff)
#   )
# # should match:
# sim_n500 %>% slice(1) %>% select(contains("bias"))
