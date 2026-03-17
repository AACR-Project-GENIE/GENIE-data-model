eval_beta_df_uni_cox <- function(
  beta_valid,
  coef_est,
  beta_thresh = 10^-6,
  select_thresh = 0.05,
  keep_select_criterion = T,
  decision_column = 'p.value'
) {
  rtn <- tibble(
    term = names(beta_valid),
    val_true = beta_valid
  )

  rtn <- rtn %>%
    mutate(is_null = abs(val_true) < beta_thresh)

  rtn <- left_join(
    rtn,
    select(coef_est, term, estimate, .data[[decision_column]]),
    by = 'term'
  )

  rtn <- rtn %>%
    mutate(
      selected = case_when(
        .data[[decision_column]] < select_thresh ~ T,
        T ~ F
      )
    )

  if (!keep_select_criterion) {
    rtn %<>% select(-vars(decision_column))
  }

  rtn %<>%
    mutate(bias = estimate - val_true, abs_bias = abs(bias))

  return(rtn)
}

eval_beta_df_lasso_once <- function(
  beta_valid,
  coef_est,
  beta_thresh = 10^-6,
  select_thresh = 10^-6
  # keep_select_criterion makes no sense here - it's all in the estimate.
) {
  rtn <- tibble(
    term = names(beta_valid),
    val_true = beta_valid
  )

  rtn <- rtn %>%
    mutate(is_null = abs(val_true) < beta_thresh)

  rtn <- left_join(
    rtn,
    select(coef_est, term, estimate),
    by = 'term'
  )

  rtn <- rtn %>%
    mutate(
      selected = case_when(
        abs(estimate) > select_thresh ~ T,
        T ~ F
      )
    )

  rtn %<>%
    mutate(bias = estimate - val_true, abs_bias = abs(bias))

  return(rtn)
}


eval_beta_df_lasso_boot <- function(
  beta_valid,
  coef_est,
  beta_thresh = 10^-6,
  select_thresh = 0.2 # "stability" aka selection frequency.
  # keep_select_criterion makes no sense here - it's all in the estimate.
) {
  rtn <- tibble(
    term = names(beta_valid),
    val_true = beta_valid
  )

  rtn <- rtn %>%
    mutate(is_null = abs(val_true) < beta_thresh)

  rtn <- left_join(
    rtn,
    select(coef_est, term, stability, estimate),
    by = 'term'
  )

  rtn <- rtn %>%
    mutate(
      selected = case_when(
        stability > select_thresh ~ T,
        T ~ F
      )
    )

  rtn %<>%
    mutate(bias = estimate - val_true, abs_bias = abs(bias))

  return(rtn)
}
