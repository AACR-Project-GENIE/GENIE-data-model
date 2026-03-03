# split the valid values spec from the data dictionary into columns for the key (coded, usually numeric, value) and the value (the meaning, which is usually a character string).
split_valid_values <- function(
  data_dictionary,
  add_to_existing = T
) {
  dat_valid_vals <- data_dictionary %>%
    filter(!is.na(valid_val_str))

  dat_valid_vals %<>%
    mutate(
      valid_val_struc = parse_valid_value_sets(valid_val_str),
      valid_val_key_code = purrr::map(.x = valid_val_struc, .f = names),
      valid_val_value_meaning = purrr::map(.x = valid_val_struc, .f = names)
    ) %>%
    select(
      field_name,
      valid_val_struc,
      valid_val_key_code,
      valid_val_value_meaning
    )

  if (add_to_existing) {
    left_join(
      data_dictionary,
      dat_valid_vals,
      by = 'field_name'
    )
  } else {
    dat_valid_vals
  }
}
