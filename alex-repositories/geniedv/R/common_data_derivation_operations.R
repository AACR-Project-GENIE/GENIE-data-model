#' Common data derivation operations
#'
#' @description
#' Currently does two things:  (1) Converts numeric columns to the value stored in the data dictionary. (2) Casts columns that are listed in as numeric in the data dictionary.
#'
#'
#' @param dat A table representing one PRISSMM instrument
#' @param dict The aligned data dictionary for this project.
#' @param print_cols Passed on to
#' `convert_dat_col_types()`.
#' @param exclude_cols Columns to exclude from the data dictionary before doing anything.
#'
#' @returns A dataset with the operations listed above completed.
#' @export
#'
#' @examples
common_data_derivation_operations <- function(
  dat,
  dict,
  print_cols = F,
  exclude_cols = NULL
) {
  if (!is.null(exclude_cols)) {
    dict <- dict %>%
      dplyr::filter(!field_name %in% exclude_cols)
  }

  rtn <- convert_dat_num2val(
    dict = dict,
    dat = dat,
    print_cols = print_cols
  )

  # Where col_read_type is numeric, cast it:
  convert_dat_col_types(
    dict = dict,
    dat = rtn,
    print_cols = print_cols
  )
}
