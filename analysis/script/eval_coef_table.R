eval_coef_2x2_cox <- function(true_beta_valid, coef_dat) {
  rtn_dat <- tibble(
    term = names(true_beta_valid),
    truth = true_beta_valid
  ) 
  
  rtn_dat <- left_join(
    rtn_dat,
    coef_dat,
    by = 'term',
    relationship = 'one-to-one'
  )
  
  rtn_dat %<>%
    mutate(
      true_non_null = abs(truth) > 10^-6,
      true_null = abs(truth) <= 10^-6,
      test_non_null = p.value < 0.05,
      test_null = p.value >= 0.05
    ) %>%
    summarize(
      # tp = true positive, tn = true negative, etc.
      tp_coef = sum(true_non_null & test_non_null),
      tn_coef = sum(true_null & test_null),
      fp_coef = sum(true_null & test_non_null),
      fn_coef = sum(true_non_null & test_non_null)
    )

  return(rtn_dat)
  
}

test_beta <- sim_n80 %>% slice(2) %>% pull(beta_valid) %>% unlist(.)
test_coef_dat <- sim_n80 %>% slice(2) %>% pull(coef_est) %>% `[[`(.,1)
eval_coef_table_cox(test_beta, coef_dat = test_coef_dat)
