tidy_cv_glmnet <- function(cv_coef, exp_coef = F, remove_zero = F) {
  rtn <- cv_coef %>%
    as.matrix(.) %>% 
    as_tibble(., rownames = "term") %>%
    rename(value = `1`)
  
  
  if (remove_zero) {
    rtn <- rtn %>%
      filter(abs(value) > 0)
  }
  
  if (exp_coef) {
    rtn <- rtn %>%
      mutate(value = exp(value)) %>%
      rename(hr = value)
  } else {
    rtn <- rtn %>%
      rename(log_hr = value)
  }
  
  return(rtn)
  
}