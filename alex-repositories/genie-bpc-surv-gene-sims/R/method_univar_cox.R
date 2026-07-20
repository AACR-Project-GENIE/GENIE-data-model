
#' @param dat A tibble/dataframe with columns for x (truncation time), y (survival/censoring time) and event (0/1 indicator for event happening).  
#' @param ignore_cols Columns to ignore - all columns that are not x, y or event will be used as covariates.
method_univar_cox <- function(dat, ignore_cols = "id_obs") {
  
  dat_y <- dat %>% select(x,y,event)
  
  dat_x <- dat %>% select(-c(x,y,event), -any_of(ignore_cols))
  
  x_numeric_chk <- dat_x %>% lapply(., FUN = is.numeric) %>% unlist %>% all
  if (!x_numeric_chk) {
    cli::cli_abort("The covariates (dat less x, y, event and ignore_cols) contains non-numeric covariates")
  }
  
  surv_obj <- with(
    dat_y,
    Surv(
      time = x,
      time2 = y,
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

method_univar_cox_helper <- function(dat, survival_object, var) {
  if (length(unique(dat[[var]])) == 2) {
    n_pos_binary <- dat %>% filter(.data[[var]] %in% 1) %>% nrow
  } else {
    n_pos_binary <- NA
  }
  
  tryCatch(
    {rtn <- coxph(
      formula = as.formula(paste0("survival_object ~ ", var)), 
      data = dat
    ) %>%
      broom::tidy(.)
    },
    error = tibble(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA)
  )
  
  rtn %<>% mutate(n_pos_binary = n_pos_binary)
  
  return(rtn)
  
}

