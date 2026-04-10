#' Add an institution column derived from record_id
#'
#' Extracts the institution as the second dash-separated token of `record_id`
#' and places it immediately after `record_id`.
#'
#' @param dat A data frame containing a `record_id` column.
#'
#' @returns `dat` with an `institution` column added after `record_id`.
#' @export
#'
#' @examples
#' # add_institution(pt)
add_institution <- function(dat) {
  dat |>
    dplyr::mutate(
      institution = stringr::word(record_id, 2, sep = "-")
    ) |>
    dplyr::relocate(institution, .after = record_id)
}
