eval_spec_at_thresh <- function(
    true_beta, 
    coef_dat, 
    thresh_param, 
    thresh_to_test,
    low_thresh_good = T
) {
  beta_zero <- true_beta[abs(true_beta) <= 0.001]
  
  coef_dat %<>%
    filter(term %in% names(beta_zero)) %>%
    select(term, estimate, all_of(thresh_param))
  
  if (low_thresh_good) {
    # for example, p values.
    coef_dat %<>%
      mutate(
        identified = .data[[thresh_param]] <= thresh_to_test
      )
  } else {
    # for example, stability.
    coef_dat %<>%
      mutate(
        identified = .data[[thresh_param]] >= thresh_to_test
      )
  }
  
  specificity = coef_dat %>% 
    mutate(identified = if_else(is.na(identified), F, identified)) %>%
    summarize(
      spec = 1-mean(identified)
    ) %>%
    pull(spec)
  
  return(specificity)
  
}
