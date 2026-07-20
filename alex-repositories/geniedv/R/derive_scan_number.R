#' Derive scan_number (imaging scan number within patient)
#'
#' Numbers imaging records within each `record_id`, ordered by
#' `image_scan_int` with ties broken by `redcap_repeat_instance`.
#'
#' @param dat An imaging-level data frame with columns `record_id`,
#'   `image_scan_int`, and `redcap_repeat_instance`.
#'
#' @returns `dat` with `scan_number` added.
#' @export
#'
#' @examples
#' # derive_scan_number(img)
derive_scan_number <- function(dat) {
  dat |>
    dplyr::group_by(record_id) |>
    dplyr::arrange(image_scan_int, redcap_repeat_instance) |>
    dplyr::mutate(scan_number = dplyr::row_number()) |>
    dplyr::ungroup()
}
