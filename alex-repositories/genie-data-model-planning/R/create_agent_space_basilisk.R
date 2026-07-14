create_agent_space_basilisk <- function(
  dat_dict,
  dat_basil,
  log_file = NULL,
  table_name = "REDCap submission",
  unique_row_def = c(
    'record_id',
    'redcap_repeat_instrument',
    'redcap_repeat_instance'
  )
) {
  req_cols <- dat_dict %>%
    filter(required) %>%
    pull(field_name)

  comp_chk_cols <- dat_dict %>%
    filter(field_type %in% "complete_check") %>%
    pull(field_name)

  # It just makes sense for these to be required too:
  req_cols <- c(req_cols, comp_chk_cols)

  if (is.null(log_file)) {
    basil_al <- action_levels(
      warn_at = 1
    )
  } else {
    basil_al <- action_levels(
      warn_at = list(
        warn = ~ log4r_step(
          x,
          append_to = log_file
        )
      )
    )
  }

  basil_agent <- dat_basil %>%
    pointblank::create_agent(
      tbl_name = table_name,
      actions = basil_al
    ) %>%
    pointblank::col_exists(columns = all_of(req_cols)) %>%
    pointblank::rows_distinct(
      columns = c(
        'record_id',
        'redcap_repeat_instrument',
        'redcap_repeat_instance'
      )
    ) %>%
    pointblank::col_vals_in_set(
      columns = comp_chk_cols,
      set = c("2", NA),
      label = "Form marked complete or column does not pertain to this form (NA)."
    )

  return(basil_agent)
}
