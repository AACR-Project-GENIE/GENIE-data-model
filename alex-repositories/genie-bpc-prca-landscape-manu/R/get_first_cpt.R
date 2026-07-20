get_first_cpt <- function(
    ca_ind_dat, 
    cpt_dat,
    type = "report"
) {
  if (type %in% 'report') {
    rtn <- ca_ind_dat %>% 
      select(record_id, ca_seq) %>%
      left_join(., cpt_dat, by = c("record_id", "ca_seq")) %>%
      arrange(cpt_number) %>%
      group_by(record_id) %>%
      slice(1) %>%
      ungroup() %>%
      select(record_id, ca_seq, dx_cpt_rep_yrs)
  } else if (type %in% 'order') {
    rtn <- ca_ind_dat %>% 
      select(record_id, ca_seq) %>%
      left_join(., cpt_dat, by = c("record_id", "ca_seq")) %>%
      arrange(cpt_number) %>%
      mutate(
        # recalculate this interval rather than requiring the pt dataset.
        dob_dx_days = dob_cpt_report_days - dx_cpt_rep_days,
        dx_cpt_order_yrs = (cpt_order_int - dob_dx_days) / 365.25
      ) %>%
      group_by(record_id) %>%
      slice(1) %>%
      ungroup() %>%
      select(record_id, ca_seq, dx_cpt_order_yrs)
    
  } else {
    cli_abort("Invalid type argumnet.")
  }
  
  return(rtn)
}
