#' Convert dataset column types
#'
#' @param dat A PRISSMM/REDcap derived dataset.
#' @param dict The data dictionary for the project.
#' @param print_cols T/F - prints a message about the columns.  Defaults to FALSE.
#'
#' @returns A version of `dat` with the column types updated.
#' @export
#'
#' @examples

convert_dat_col_types <- function(
  dat,
  dict,
  print_cols = F
) {
  common_cols <- intersect(colnames(dat), dict$field_name)

  manifest <- dict %>%
    dplyr::filter(field_name %in% common_cols) %>%
    dplyr::mutate(
      letter_code = dplyr::case_when(
        col_read_type %in% "char" ~ 'c',
        col_read_type %in% "numeric" ~ 'd',
        col_read_type %in% "logical" ~ 'l',
        col_read_type %in% 'dttm' ~ 'T',
        T ~ "ERROR"
      )
    )

  if (any(manifest$letter_code %in% "ERROR")) {
    first_error <- manifest %>%
      dplyr::filter(letter_code %in% "ERROR") %>%
      dplyr::slice(1) %>%
      dplyr::pull(col_read_type)
    cli::cli_abort(
      'Unrecognized col_read_type "{first_error}"- edit function to add.'
    )
  }

  if (print_cols) {
    cli::cli_inform(
      "Converting {length(common_cols)} columns: {paste(common_cols, collapse = ', ')}"
    )
  }

  col_spec = as.list(manifest$letter_code)
  names(col_spec) <- manifest$field_name
  col_spec <- do.call(readr::cols, args = col_spec)

  rtn <- readr::type_convert(
    dat,
    col_types = col_spec
  )

  rtn
}
