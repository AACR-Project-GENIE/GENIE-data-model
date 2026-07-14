eval_coef_2x2 <- function(beta_dat) {

  rtn <- beta_dat %>%
    summarize(
      tp_beta = sum(!is_null & selected),
      tn_beta = sum(is_null & !selected),
      fp_beta = sum(is_null & selected),
      fn_beta = sum(!is_null & !selected)
    )

  return(rtn)
  
}

# power is aka probability of detection, hit rate, sensitivity, true positive rate, and recall..
# big confusion matrix here: https://en.wikipedia.org/wiki/Precision_and_recall
eval_power <- function(
    tp_beta, fn_beta
) {
  return(tp_beta / (tp_beta + fn_beta))
}

# 1-alpha = specificity, selectivity, true negative rate.  I think of this as the empirical confidence level.
eval_selectivity <- function(
    tn_beta, fp_beta
) {
  return(tn_beta / (tn_beta + fp_beta))
}

# test_beta <- sim_n80 %>% slice(2) %>% pull(beta_valid) %>% unlist(.)
# test_coef_dat <- sim_n80 %>% slice(2) %>% pull(coef_est) %>% `[[`(.,1)
# eval_coef_table_cox(test_beta, coef_dat = test_coef_dat)
