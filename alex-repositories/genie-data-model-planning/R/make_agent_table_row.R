make_agent_table_row <- function(
  dat_dict,
  interrogated_agent,
  qc_layer = NA_real_,
  site_to_qc = NA_character_
) {
  qc_dat <- tibble(
    qc_layer = qc_layer,
    site = site_to_qc, # redundant with folder but that's OK.
    dat_dict = list(dat_dict),
    dat_name = interrogated_agent$tbl_name,
    report = list(get_agent_report(
      interrogated_agent,
      display_table = F,
      keep = 'all',
      size = 'small'
    )),
    all_passed = all_passed(interrogated_agent),
    # could probably combine the extracts and validation sets but not required now really.
    validation_subset = list(get_validation_subset(interrogated_agent)),
    extracts = list(interrogated_agent$extracts),
    time_start = interrogated_agent$time_start,
    time_end = interrogated_agent$time_end,
  )

  qc_dat
}

get_validation_subset <- function(
  intel_obj
) {
  intel_obj$validation_set %>%
    select(
      assertion_type,
      columns_expr,
      brief,
      label,
      i,
      time_processed
      # sha1 seemed useful too, removed it though as I'm not totally clear what the hash inputs are.
    )
}
