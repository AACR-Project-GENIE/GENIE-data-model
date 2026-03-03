# split the valid values spec from the data dictionary into columns for the key (coded, usually numeric, value) and the value (the meaning, which is usually a character string).
#' Split the valid values into structured columns.
#'
#' @param data_dictionary The data dictionary with `valid_val_str` already added (for exmaple with `dd_readr()`).
#' @param add_to_existing Add the derived columns to the existing data?
#'
#' @returns A tibble like `data_dictionary` with the new columns if add_to_existing = TRUE.  Otherwise just the new cols.
#' @export
#'
#' @examples
split_valid_values <- function(
  data_dictionary,
  add_to_existing = T
) {
  dat_valid_vals <- data_dictionary %>%
    filter(!is.na(valid_val_str))

  dat_valid_vals %<>%
    dplyr::mutate(
      valid_val_struc = parse_valid_value_sets(valid_val_str),
      valid_val_key_code = purrr::map(.x = valid_val_struc, .f = names),
      valid_val_value_meaning = purrr::map(.x = valid_val_struc, .f = names)
    ) %>%
    dplyr::select(
      field_name,
      valid_val_struc,
      valid_val_key_code,
      valid_val_value_meaning
    )

  if (add_to_existing) {
    dplyr::left_join(
      data_dictionary,
      dat_valid_vals,
      by = 'field_name'
    )
  } else {
    dat_valid_vals
  }
}
