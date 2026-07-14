
eval_avg_bias <- function(
    beta_dat,
    absolute = F,
    inclusion = "all"
) {
  if (inclusion %in% "all") {
    # do nothing
  } else if (inclusion %in% "selected") {
    beta_dat <- filter(beta_dat, selected)
  } else if (inclusion %in% "true_effects") {
    beta_dat <- filter(beta_dat, !is_null)
  } else {
    cli::cli_abort("Unknown inclusion type")
  }
  
  if (absolute) {
    rtn <- beta_dat %>%
      summarize(avg = mean(abs_bias))
  } else {
    rtn <- beta_dat %>%
      summarize(avg = mean(bias))
  }
  rtn <- pull(rtn, avg)
  
  return(rtn)
  
} 