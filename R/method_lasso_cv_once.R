method_lasso_cv_once <- function(
    dat,
    seed,
    ignore_cols = "id_obs",
    cv_folds = 5,
    x_col = "x",
    y_col = "y",
    event_col = "event",
    lambda_for_coef_est = "lambda.min"
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
  
  set.seed(seed)
  cv_fit <- tryCatch(
    expr = {
      cv.glmnet(
        x = x_mat,
        y = y_surv,
        standardize = T,
        alpha = 0.98, # Small amount of elastic net for convergence.
        family = "cox",
        nfolds = cv_folds
      )
    },
    error = function(e) {NULL}
  )
  
  if (is.null(cv_fit)) {
    coef_est <- NULL
    dropout_dat <- NULL
  } else {
    coef_est <- get_cv_lasso_coefs(
      las_fit = cv_fit,
      lambda_value = lambda_for_coef_est
    )
    dropout_dat <- cv_fit %>%
      get_coefs_all_lambda(.) %>%
      find_dropout_lambda(.)
  }
  
  return(
    tibble(
      fit = list(cv_fit), 
      coef_est = list(coef_est),
      dropout_dat = list(dropout_dat)
    )
  )
  
}