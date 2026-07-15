
#' @title Get cancer panel test by time
#' @description Get all cancer panel tests BEFORE the time given in 
#'   time_dat$time_var, or any tests which are the first cancer panel test. 
#'   Filtering is also limited only to {record_id, ca_seq} pairs in
#'   time_dat.
#' @details 
get_cpt_by_time <- function(
    time_dat,
    time_var,
    cpt_dat,
    time_tol = 0,
    always_keep_first = T) {
  if (!all(c('record_id', 'ca_seq', time_var) %in% names(time_dat))) {
    cli::cli_abort(
      "time_dat must have columns record_id, ca_seq and 'time_var'."
      )
  }
  if (any(time_dat[[time_var]] > 100)) {
    cli::cli_abort(
      "time_var is assumed to be in years, but values over 100 were detected.  Wrong units?"
    )
  }
  
  time_dat <- time_dat %>%
    select(record_id, ca_seq, all_of(time_var))
  time_dat_max <- time_dat %>% 
    count(record_id, ca_seq, sort = T) %>% 
    pull %>% 
    max
  if (time_dat_max > 1) {
    cli_abort("More than one record per record_id, ca_seq in time_dat - these should be unique!")
  }
  
  rtn_dat <- inner_join(
    cpt_dat,
    time_dat,
    by = c("record_id", "ca_seq")
  )
  
  rtn_dat %<>%
    group_by(record_id, ca_seq) %>%
    arrange(dx_cpt_rep_yrs) %>%
    mutate(
      is_first_cpt = row_number() %in% 1,
      cpt_before_t = dx_cpt_rep_yrs <= .data[[time_var]] + time_tol
    ) %>%
    ungroup(.) 
  
  rtn_dat %<>% select(-all_of(time_var))
  
  if (always_keep_first) {
    rtn_dat %<>% filter(is_first_cpt | cpt_before_t)
  } else {
    rtn_dat %<>% filter(cpt_before_t)
  }
  
  return(rtn_dat)
}
