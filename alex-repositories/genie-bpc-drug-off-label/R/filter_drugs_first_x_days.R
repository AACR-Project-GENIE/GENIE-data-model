filter_drugs_first_x_days <- function(
    dat_drug,
    var_index_time,
    var_start_time = 'dx_drug_start_int',
    x = 0,
    tol = 0.5
) {
  dat_drug %>%
    mutate(
      .diff = .data[[var_start_time]] - .data[[var_index_time]],
      .in_window = (.diff > -tol) & (.diff < (x + tol))
    ) %>%
    filter(.in_window) %>%
    select(-.diff, -.in_window)
  
}

# Example use:
# dft_hdrug_fxd %>%
#   filter(record_id %in% "GENIE-DFCI-000061") %>%
#   filter_drugs_first_x_days(
#     ., x = 30,
#     var_index_time = 'dx_first_drug_use_int'
#   )
