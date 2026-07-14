#' Derive cpt_number (sequencing report number within patient)
#'
#' Numbers CPT records within each `record_id`, ordered by
#' `dob_cpt_report_days` with ties broken by `redcap_repeat_instance`.
#'
#' @param dat A CPT-level data frame with columns `record_id`,
#'   `dob_cpt_report_days`, and `redcap_repeat_instance`.
#'
#' @returns `dat` with `cpt_number` added.
#' @export
#'
#' @examples
#' # derive_cpt_number(cpt)
derive_cpt_number <- function(dat) {
  dat |>
    dplyr::group_by(record_id) |>
    dplyr::arrange(dob_cpt_report_days, redcap_repeat_instance) |>
    dplyr::mutate(cpt_number = dplyr::row_number()) |>
    dplyr::ungroup()
}
