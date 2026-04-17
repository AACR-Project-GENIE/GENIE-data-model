#' Derive path_proc_number (pathology procedure number within patient)
#'
#' Numbers pathology procedures within each `record_id` using dense rank on
#' `path_proc_int`, so records sharing the same procedure date get the same
#' number.
#'
#' @param dat A pathology-level data frame with columns `record_id` and
#'   `path_proc_int`.
#'
#' @returns `dat` with `path_proc_number` added.
#' @export
#'
#' @examples
#' # derive_path_proc_number(path)
derive_path_proc_number <- function(dat) {
  dat |>
    dplyr::group_by(record_id) |>
    dplyr::mutate(path_proc_number = dplyr::dense_rank(path_proc_int)) |>
    dplyr::ungroup()
}
