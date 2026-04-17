#' Derive path_proc_number (pathology procedure number within patient)
#'
#' Numbers pathology records within each `record_id`, ordered by
#' `path_proc_int` with ties broken by `redcap_repeat_instance`.
#'
#' @param dat A pathology-level data frame with columns `record_id`,
#'   `path_proc_int`, and `redcap_repeat_instance`.
#'
#' @returns `dat` with `path_proc_number` added.
#' @export
#'
#' @examples
#' # derive_path_proc_number(path)
derive_path_proc_number <- function(dat) {
  dat |>
    dplyr::group_by(record_id) |>
    dplyr::arrange(path_proc_int, redcap_repeat_instance) |>
    dplyr::mutate(path_proc_number = dplyr::row_number()) |>
    dplyr::ungroup()
}
