#' @details Pulls the progression times for participants who experienced
#'    progression (but not death).  For exmaple, if you a user input
#'    prefix = "pfs_i_or_m_adv" then the function returns progression times
#'    for participants under that event.  The progression times are 
#'    tt_pfs_i_or_m_adv_\*, where \* is days, mos or yrs.
#'    
#'    This function would never be appropriate
#'    for survival.  It's intended purpose is 
#' @param ca_dat A BPC index cancer dataset.
#' @param prefix A character vector for the event/time variables of interest (example: prefix = "pfs_i_or_m_adv")
get_progressed_timing <- function(ca_dat, prefix) {
  
  stat_var <- paste0(prefix, '_status')
  tt_d <- paste0('tt_', prefix, '_days')
  tt_m <- paste0('tt_', prefix, '_mos')
  tt_y <- paste0('tt_', prefix, '_yrs')
  
  ca_dat %<>%
    as_tibble(.) %>%
    filter(.data[[stat_var]] %in% 1) %>%
    # A couple of quick variables to make the filter more readable:
    mutate(
      did_not_die = !(os_dx_status %in% 1),
      pfs_time_lt_os_time = case_when(
        is.na(.data[[tt_d]]) ~ T,
        is.na(tt_os_dx_days) ~ T,
        .data[[tt_d]] < tt_os_dx_days ~ T,
        T ~ F
      )
    ) %>% 
    filter(did_not_die | pfs_time_lt_os_time) %>%
    mutate(
      # we give these arbitrary names for use in future functions
      tt_d = .data[[tt_d]],
      tt_m = .data[[tt_m]],
      tt_y = .data[[tt_y]],
      fn_prefix = prefix # save the prefix input.
    ) %>%
    select(record_id,  ca_seq, tt_d, tt_m, tt_y, fn_prefix)
  
  # For cases where we have more than one row in ca_dat (rare but real), 
  #  we can sensibly grab only the first one.
  ca_dat %<>% 
    group_by(record_id) %>%
    arrange(tt_d) %>%
    slice(1) %>%
    ungroup
  
  return(ca_dat)
}


# Tests:

# get_progressed_timing(ca_dat = dft_ca_dx_index, 
#                       prefix = "pfs_i_or_m_adv") %>% glimpse

# Should be smaller:
# get_progressed_timing(ca_dat = ca_dx_index, 
#                       prefix = "pfs_i_and_m_adv") %>% glimpse
# # Goldilocks options:
# get_progressed_timing(ca_dat = ca_dx_index, 
#                       prefix = "pfs_i_adv") %>% glimpse
# get_progressed_timing(ca_dat = ca_dx_index, 
#                       prefix = "pfs_m_adv") %>% glimpse