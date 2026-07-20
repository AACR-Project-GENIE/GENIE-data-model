#' @title Check uniqueness of rows in primary_keys
#'
#' @param dat A tibble/df that has the columns in primary_keys.
#' @param primary_keys A character vector containing the primary keys.
#' @export
check_unique_rows <- function(
  dat,
  primary_keys
) {
  nr_orig <- nrow(dat)

  nr_keys <- dat %>%
    dplyr::select(dplyr::all_of(primary_keys)) %>%
    distinct(.) %>%
    nrow(.)

  key_def_good <- nr_orig == nr_keys

  if (!key_def_good) {
    cli::cli_abort(
      "dataset does not have a unique row definition by primary keys"
    )
  }
  invisible(key_def_good)
}
