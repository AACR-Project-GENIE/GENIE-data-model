
#' @description Evaluate the AUROC for beta coefficients using p values as the decision threshold variable.
#' @param true_beta The true beta values.  
#' @param coef_dat A dataframe with coefficient estimates.  Required columns are "term" and "p.value".
eval_beta_auc_pval <- function(true_beta, coef_dat, return_type = "auc") {
  dec_vals <- split_decision_values(
    true_beta = true_beta,
    coef_dat = coef_dat,
    d_name = "p.value"
  )
  
  t_pos_pval <- dec_vals[['t_pos']]
  t_neg_pval <- dec_vals[['t_neg']]
  
  # For this ROC package we need high scores to indicate high selection probability, so we just reverse the P values:
  t_pos_pval_rev = 1 - t_pos_pval
  t_neg_pval_rev = 1 - t_neg_pval
  
  roc <- roc.curve(
    scores.class0 = t_pos_pval_rev,
    scores.class1 = t_neg_pval_rev,
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
