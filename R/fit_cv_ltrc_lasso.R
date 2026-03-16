fit_cv_ltrc_lasso <- function(
    x_mat, 
    y_df,  
    y_t, 
    y_t2, 
    y_event,
    cv_folds = 5) {

  y_surv <- Surv(
    time = y_df[[y_t]],
    time2 = y_df[[y_t2]],
    event = y_df[[y_event]]
  )
  
  cv_fit <- cv.glmnet(
    x = x_mat,
    y = y_surv,
    standardize = T,
    alpha = 0.98, # Small amount of elastic net for convergence.
    family = "cox",
    nfolds = cv_folds
  )
  
  return(cv_fit)
}
