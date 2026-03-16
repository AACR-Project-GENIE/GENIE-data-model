
#' @description Evaluate the AUROC for beta coefficients using p values as the decision threshold variable.
#' @param true_beta The true beta values.  
#' @param coef_dat A dataframe with coefficient estimates.  Required columns are "term" and "p.value".
eval_beta_auc_stability <- function(
    true_beta, 
    coef_dat, 
    return_type = "auc"
) {
  dec_vals <- split_decision_values(
    true_beta = true_beta,
    coef_dat = coef_dat,
    d_name = "stability"
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
