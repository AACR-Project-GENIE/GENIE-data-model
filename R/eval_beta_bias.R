
#' @description Evaluate the bias between some true betas and a list of coefficient estimates.
#' @param true_beta_valid The list of true betas you want to evaluate on.  The assumption is that any beta which is not zero (tolerance 0.001) will be a "true effect.
#' @param coef_dat The list of coefficents and their estimates.  Requried columns: 'term' and 'estimate'.
eval_beta_bias <- function(true_beta_valid, coef_dat) {
  beta_nonzero <- true_beta_valid[abs(true_beta_valid) > 0.001]
  
  coef_dat %<>%
    filter(term %in% names(beta_nonzero)) %>%
    select(term, estimate)
  
  beta_dat <- tibble(
    term = names(beta_nonzero),
    truth = beta_nonzero
  )
  
  bias_dat <- left_join(
    beta_dat,
    coef_dat,
    by = "term",
    relationship = "one-to-one"
  )
  
  if (any(is.na(bias_dat$estimate))) {
    cli::cli_alert_warning("Some terms were not found in coef_dat")
  }
  
  bias_dat %<>% mutate(diff = estimate - truth)
  
  bias_dat %<>%
    summarize(
      avg_abs_bias = mean(abs(diff), na.rm = T),
      avg_bias = mean(diff, na.rm = T),
      se_bias = sd(diff, na.rm = T)
    )
  
  if (nrow(bias_dat) > 1) {
    cli::cli_abort("Something went wrong, return dataframe has multiple rows.")
  }
  
  return(bias_dat) # returns a dataframe, so you'll have to unpack it.
  
}
