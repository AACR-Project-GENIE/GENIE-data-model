
#' @description Returns the decision boundary values for the true positives and true negatives.  TP and TN are betas that are exactly zero := truly negative.  Required to have names so we can check against the coef_dat.
#' @param true_beta
#' @param coef_dat
split_decision_values <- function(true_beta, coef_dat, d_name = "p.value") {
  if (length(setdiff(names(true_beta), coef_dat$term)) > 0) {
    stop("The true_beta names don't match the terms in coef_dat")
  }
  
  if (!(d_name %in% names(coef_dat))) {
    cli::cli_abort("d_name is not a column in coef_dat.")
  }
  
  # t_pos = true positive, t_neg = true negative
  t_pos_name <- names(true_beta[abs(true_beta) > 0.0001])
  t_neg_name <- names(true_beta[abs(true_beta) <= 0.0001])
  
  t_pos_val <- coef_dat %>%
    filter(term %in% t_pos_name) %>%
    # Decision:  We will remove terms where no estimate could be obtained.
    filter(!is.na(.data[[d_name]])) %>%
    pull(.data[[d_name]])
  
  t_neg_val <- coef_dat %>%
    filter(term %in% t_neg_name) %>%
    # Decision:  We will remove terms where no estimate could be obtained.
    filter(!is.na(.data[[d_name]])) %>%
    pull(.data[[d_name]])
  
  return(list(t_pos = t_pos_val, t_neg = t_neg_val))
}
