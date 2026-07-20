
# Cast_block_dat is the output from make_cast_status_block()
make_event_df <- function(ca_ind_dat, 
                          cpt_dat,
                          cast_block_dat) {
  
  # for each index cancer get the first cpt
  first_panel_dates <- get_first_cpt(ca_ind_dat, cpt_dat) %>%
    select(record_id, t_yrs = dx_cpt_rep_yrs)
  
  event_dat <- cast_block_dat %>%
    select(record_id, t_yrs = tt_os_dx_yrs, event = os_dx_status_f) %>%
    mutate(event = as.character(event)) %>%
    bind_rows(., first_panel_dates) %>%
    mutate(
      event = if_else(is.na(event), "First CPT", event),
      event = factor(event, levels = c("First CPT", "Censored", "Death"))
    )
  
  return(event_dat)
}