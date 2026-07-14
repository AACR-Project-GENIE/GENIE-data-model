eval_sens_stabsel <- function(true_beta, coef_dat, beta_tol = 0.001) {
  beta_nonzero <- true_beta[abs(true_beta) > beta_tol]

  coef_dat %>%
    filter(variable %in% names(beta_nonzero)) %>%
    mutate(selected = if_else(is.na(selected), FALSE, selected)) %>%
    summarize(sens = mean(selected)) %>%
    pull(sens)
}

eval_spec_stabsel <- function(true_beta, coef_dat, beta_tol = 0.001) {
  beta_zero <- true_beta[abs(true_beta) <= beta_tol]

  coef_dat %>%
    filter(variable %in% names(beta_zero)) %>%
    mutate(selected = if_else(is.na(selected), FALSE, selected)) %>%
    summarize(spec = 1 - mean(selected)) %>%
    pull(spec)
}
