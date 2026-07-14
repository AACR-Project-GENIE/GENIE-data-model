#' @description 
#' @param true_beta A vector of beta values (or subset you consider valid for the model to evaluate)
#' @param dropout_dat A dataset specifying that lambda value at which each feature dropped out of the model (i.e. the term went to zero).
eval_beta_auc_lambda <- function(
    true_beta, 
    dropout_dat, 
    return_type = "auc"
) {
  dec_vals <- split_decision_values(
    true_beta = true_beta,
    coef_dat = dropout_dat,
    d_name = "lambda"
  )
  
  t_pos_val <- dec_vals[['t_pos']]
  t_neg_val <- dec_vals[['t_neg']]
  
  roc <- roc.curve(
    scores.class0 = t_pos_val,
    scores.class1 = t_neg_val,
    curve = T
  )
  
  if (return_type %in% "auc") {
    return(roc$auc)
  } else if (return_type %in% "plot") {
    cli::cli_alert_danger("The color scale shows 1-PVal")
    return(plot(roc))
  } else {
    cli::cli_alert_danger("Return type not 'auc' or 'plot' so returning the whole object")
    return(roc)
  }
  
}
