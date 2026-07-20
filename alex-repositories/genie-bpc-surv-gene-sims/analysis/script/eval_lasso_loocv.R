# Description: Runs the univariate cox method on simulated datasets.

library(fs); library(here); library(purrr)
purrr::walk(.x = fs::dir_ls('R'), .f = source)

sim_n80 <- readr::read_rds(
  here('sim', 'run_methods',
       'gen_dat_one_n80_lasso_loocv.rds')
)

sim_n500 <- readr::read_rds(
  here('sim', 'run_methods', 
       'gen_dat_one_n500_lasso_loocv.rds')
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

# a few of these returned a NULL value
# solution for now is checking that the proportion with this error is less than 5% (it was 0.15% last manual check) and removing them.
null_prop <- c((sim_n80 %>% pull(coef_est) %>% purrr::map_lgl(is.null)),
    (sim_n500 %>% pull(coef_est) %>% purrr::map_lgl(is.null))
) %>% mean
if (null_prop < 0.05) {
  sim_n80 %<>% filter(!purrr::map_lgl(coef_est,is.null))
  sim_n500 %<>% filter(!purrr::map_lgl(coef_est,is.null))
} else {
  cli_abort(">5% null values in coef_est column - needs to be looked at")
}

sim_n80 %<>% fix_coef_dat_apply(.)
sim_n500 %<>% fix_coef_dat_apply(.)
  


###########################
# Add in AUC calculation: #
###########################


eval_wrap_lasso_loocv <- function(sim_data) {
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
  
  
  
  # sim_data %<>%
  #   mutate(
  #     bias_dat = purrr::map2(
  #       .x = beta_valid,
  #       .y = coef_est,
  #       .f = (function(b, c) {
  #         # I didn't save the names right on the first run:
  #         if ("log_hr" %in% names(c)) {
  #           c %<>% rename(estimate = log_hr)
  #         }
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
      beta_dat = purrr::map2(
        .x = beta_valid,
        .y = coef_est,
        .f = (function(b, c) {
          eval_beta_df_lasso_once(
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
}
  
sim_n80 %<>% eval_wrap_lasso_loocv(.)
sim_n500 %<>% eval_wrap_lasso_loocv(.)



readr::write_rds(
  x = sim_n80,
  file = here('sim', 'evaled_methods', 'gen_dat_one_n80_lasso_loocv.rds')
)

readr::write_rds(
  x = sim_n500,
  file = here('sim', 'evaled_methods', 'gen_dat_one_n500_lasso_loocv.rds')
)


