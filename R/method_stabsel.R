method_stabsel <- function(
  dat,
  seed,
  ignore_cols = "id_obs",
  x_col = "x",
  y_col = "y",
  event_col = "event",
  cutoff = 0.75,
  PFER = 1,
  nsub = 10
) {
  dat <- dat %<>% select(-all_of(ignore_cols))

  y_dat <- dat %>%
    select(all_of(c(x_col, y_col, event_col)))
  x_mat <- dat %>%
    select(-all_of(c(x_col, y_col, event_col))) %>%
    as.matrix(.)

  y_surv <- Surv(
    time = y_dat[[x_col]],
    time2 = y_dat[[y_col]],
    event = y_dat[[event_col]]
  )

  fit <- stabsel_glmnet_q_cap(
    x_mat,
    y_surv,
    cutoff = cutoff,
    PFER = PFER,
    nsub = nsub,
    verbose = FALSE
  )

  tibble(
    variable = names(fit$sel_prob),
    selected = names(fit$sel_prob) %in% fit$stable,
    mean_coef = fit$beta_mean,
    sel_prob = fit$sel_prob
  )
}
