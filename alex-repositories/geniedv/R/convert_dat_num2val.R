#' Convert the numeric values in the data to the values listed in the data dictionary
#'
#' @param dat A PRISSMM/REDCap derived dataset.
#' @param dict Aligned data dictionary.
#' @param col_to_convert The column to convert.
#' @param print_cols Print the columns being worked on?  Default to TRUE.
#' @param remove_names Passed on to `redcap_numbers_to_meaning()`.
#'
#' @returns A dataset with the col_to_convert updated according to the dictionary values.
#' @export
#'
#' @examples
convert_dat_num2val <- function(
  dat,
  dict,
  col_to_convert = NULL,
  print_cols = T,
  remove_names = F
) {
  if (is.null(col_to_convert)) {
    col_to_convert <- dict %>%
      dplyr::filter(
        # need the purrr call because this is a list column.
        purrr::map_lgl(.x = valid_val_struc, .f = ~ !is.null(.x))
      ) %>%
      dplyr::pull(field_name)
  }

  col_not_in_data <- setdiff(col_to_convert, colnames(dat))
  col_to_convert <- intersect(col_to_convert, colnames(dat))
  if (length(col_not_in_data) > 0 & print_cols) {
    cli::cli_abort(
      "Ignoring columns not in the data: {paste(col_not_in_data, collapse = ', ')}"
    )
  }

  if (print_cols) {
    cli::cli_inform(
      "Converting {length(col_to_convert)} columns: {paste(col_to_convert, collapse = ', ')}"
    )
  }

  # A for loop is probably the easiest way to do this since we need to pluck the valid values and replace them in such different structures.
  for (working_col in col_to_convert) {
    kvp <- dict %>%
      dplyr::filter(field_name %in% working_col) %>%
      dplyr::pull(valid_val_struc) %>%
      .[[1]]

    dat <- dat %>%
      dplyr::mutate(
        '{working_col}' := redcap_numbers_to_meaning(
          vec = .data[[working_col]],
          key_val_pairs = kvp,
          remove_names = remove_names
        )
      )
  }

  dat
}
