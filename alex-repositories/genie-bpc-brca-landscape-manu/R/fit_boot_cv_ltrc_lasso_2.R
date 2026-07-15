fit_boot_cv_ltrc_lasso_2 <- function(
    dat,  
    x_col, 
    y_col, 
    event_col,
    boot_seed,
    cv_folds = 5, 
    ignore_cols = "id_obs") {
  
  dat <- dat %<>% select(-all_of(ignore_cols))
  
  b_dat <- boot_fold_help(
    dat = dat,
    seed = boot_seed,
    n_folds = cv_folds
  )
  
  fold_id <- b_dat$fold
  b_dat %<>% select(-fold)
  
  y_dat <- b_dat %>% select(all_of(c(x_col, y_col, event_col)))
  x_mat <- b_dat %>% select(-all_of(c(x_col, y_col, event_col))) %>%
    as.matrix(.)
  
  y_surv <- Surv(
    time = y_dat[[x_col]],
    time2 = y_dat[[y_col]],
    event = y_dat[[event_col]]
  )
  
  cv_fit <- tryCatch(
    expr = {
      cv.glmnet(
        x = x_mat,
        y = y_surv,
        standardize = T,
        alpha = 0.98, # Small amount of elastic net for convergence.
        family = "cox",
        foldid = fold_id
      )
    },
    error = function(e) {NULL}
  )
  
  return(cv_fit)
}