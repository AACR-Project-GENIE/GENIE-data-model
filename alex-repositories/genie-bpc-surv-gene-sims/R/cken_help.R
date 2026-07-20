cken_help <- function(dat, 
                      trunc, 
                      fail, 
                      event,
                      method = "MB",
                      ci = T) {
  
  rtn <- cKendall(
    trun = dat[[trunc]],
    obs = dat[[fail]],
    delta = dat[[event]],
    method = method
  ) %>%
    `[`(.,-1) %>%
    as_tibble(.)
  
  rtn %<>%
    mutate(
      est = PE,
      lcb = est - qnorm(0.975)*SE,
      ucb = est + qnorm(0.975)*SE
    ) %>%
    select(est, lcb, ucb, se = SE, p_value = p.value)
  
 
  return(rtn)
  
}

# Example use:
# cken_help(
#   dat = surv_dat,
#   trunc = "dx_cpt_rep_yrs",
#   fail = "tt_os_dx_yrs",
#   event = "os_dx_status",
# )