# This is a super specific function that can go away for the broader simulation project.
#' @param dat A tibble/dataframe with columns for x (truncation time), y (survival/censoring time) and event (0/1 indicator for event happening).  
#' @param ignore_cols Columns to ignore - all columns that are not x, y or event will be used as covariates.
method_univar_cox_dod_prostate <- function(dat, ignore_cols = "id") {
  
  dat_y <- dat %>% select(y,event)
  
  dat_x <- dat %>% select(-c(y,event), -any_of(ignore_cols))
  
  x_numeric_chk <- dat_x %>% lapply(., FUN = is.numeric) %>% unlist %>% all
  if (!x_numeric_chk) {
    cli::cli_abort("The covariates (dat less x, y, event and ignore_cols) contains non-numeric covariates")
  }
  
  surv_obj <- with(
    dat_y,
    Surv(
      time = y,
      event = event
    )
  )
  
  x_var_names <- names(dat_x)
  
  rtn <- purrr::map_dfr(
    .x = x_var_names,
    .f = (function(v) {
      method_univar_cox_helper(dat = dat_x, survival_object = surv_obj, var = v)
    })
  )
  
  return(rtn)
  
}
