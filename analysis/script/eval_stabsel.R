# Description: Evaluates stability selection results on simulated datasets.

library(fs)
library(here)
library(purrr)

purrr::walk(.x = fs::dir_ls('R'), .f = source)

sim_n80 <- readr::read_rds(
  here('sim', 'run_methods', 'gen_dat_one_n80_stabsel.rds')
)

sim_n500 <- readr::read_rds(
  here('sim', 'run_methods', 'gen_dat_one_n500_stabsel.rds')
)

sim_n80 %<>% unnest(fit)
sim_n500 %<>% unnest(fit)


eval_wrap_stabsel <- function(sim_data) {
  # TODO: AUC - stabsel doesn't produce continuous scores in the same way as
  # lasso; sel_prob is the natural ranking score to pass to eval_beta_auc_lambda
  # or an equivalent that accepts a per-variable probability instead of a coef.
  # sim_data %<>%
  #   mutate(
  #     auc = purrr::map2_dbl(
  #       .x = beta_valid,
  #       .y = dropout_dat,
  #       .f = (function(b, d) {
  #         eval_beta_auc_lambda(true_beta = b, dropout_dat = d, return_type = "auc")
  #       })
  #     )
  #   )

  # Sens / spec at the cutoff threshold (variable selected = TRUE/FALSE)
  # TODO: wire up to eval_spec_at_thresh / eval_sens_at_thresh once we confirm
  # how coef_dat should look for stabsel (column `selected` instead of `estimate`)
  # sim_data %<>%
  #   mutate(
  #     spec_thresh = "stabsel_cutoff",
  #     spec_at_thresh = purrr::map2_dbl(
  #       .x = beta_valid,
  #       .y = coef_est,
  #       .f = (function(b, d) {
  #         eval_spec_at_thresh(
  #           true_beta = b,
  #           coef_dat = d,
  #           thresh_param = "selected",
  #           thresh_to_test = TRUE,
  #           low_thresh_good = FALSE
  #         )
  #       })
  #     ),
  #     sens_at_thresh = purrr::map2_dbl(
  #       .x = beta_valid,
  #       .y = coef_est,
  #       .f = (function(b, d) {
  #         eval_sens_at_thresh(
  #           true_beta = b,
  #           coef_dat = d,
  #           thresh_param = "selected",
  #           thresh_to_test = TRUE,
  #           low_thresh_good = FALSE
  #         )
  #       })
  #     )
  #   )

  # TODO: beta_dat construction - need an eval_beta_df_stabsel (or adapt the
  # existing one) that joins on `variable` and uses `selected` as the inclusion
  # indicator and `mean_coef` as the estimate.
  # sim_data %<>%
  #   mutate(
  #     beta_dat = purrr::map2(
  #       .x = beta_valid,
  #       .y = coef_est,
  #       .f = (function(b, c) {
  #         eval_beta_df_stabsel(beta_valid = b, coef_est = c)
  #       })
  #     )
  #   )

  # 2x2 counts and power/selectivity - structure is the same as lasso once
  # beta_dat is built above.
  # sim_data %<>%
  #   mutate(
  #     coef_2x2_dat = purrr::map(
  #       .x = beta_dat,
  #       .f = \(b) eval_coef_2x2(beta_dat = b)
  #     )
  #   ) %>%
  #   unnest(coef_2x2_dat)
  #
  # sim_data %<>%
  #   mutate(
  #     power       = eval_power(tp_beta = tp_beta, fn_beta = fn_beta),
  #     selectivity = eval_selectivity(tn_beta = tn_beta, fp_beta = fp_beta)
  #   )

  # TODO: bias - mean_coef is the natural estimate; same eval_avg_bias calls
  # apply once beta_dat is available.
  # sim_data %<>%
  #   mutate(
  #     avg_abs_bias          = purrr::map_dbl(.x = beta_dat, \(b) eval_avg_bias(b, absolute = TRUE,  inclusion = "all")),
  #     avg_abs_bias_selected = purrr::map_dbl(.x = beta_dat, \(b) eval_avg_bias(b, absolute = TRUE,  inclusion = "selected")),
  #     avg_bias              = purrr::map_dbl(.x = beta_dat, \(b) eval_avg_bias(b, absolute = FALSE, inclusion = "all")),
  #     avg_bias_selected     = purrr::map_dbl(.x = beta_dat, \(b) eval_avg_bias(b, absolute = FALSE, inclusion = "selected"))
  #   )

  sim_data
}

sim_n80  %<>% eval_wrap_stabsel(.)
sim_n500 %<>% eval_wrap_stabsel(.)


readr::write_rds(
  x = sim_n80,
  file = here('sim', 'evaled_methods', 'gen_dat_one_n80_stabsel.rds')
)

readr::write_rds(
  x = sim_n500,
  file = here('sim', 'evaled_methods', 'gen_dat_one_n500_stabsel.rds')
)
