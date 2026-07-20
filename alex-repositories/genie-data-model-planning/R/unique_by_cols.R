#' Is this dataset unique according to the columns provided?
#'
#' @param dat
#' @param ... Columns to define uniqueness.
#'
#' @returns TRUE if the dataset is unique according to those columns and FALSE otherwise.
#' @export
#'
#' @examples
unique_by_cols <- function(dat, ...) {
  !any(duplicated(dplyr::select(dat, ...)))
}
