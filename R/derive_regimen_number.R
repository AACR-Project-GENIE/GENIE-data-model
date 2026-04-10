#' Derive regimen_number
#'
#' Orders cancer-directed regimens within each patient by the start date of
#' the first cancer-directed drug (`drugs_startdt_int_1`) and assigns
#' `regimen_number = 1, 2, ..., n`. Rows that belong to the same regimen but
#' are associated with multiple cancer diagnoses share the same
#' `regimen_number` (they are keyed together by `redcap_repeat_instance`).
#'
#' @param dat A regimen-level data frame with one row per
#'   regimen-associated cancer diagnosis. Must contain `record_id`,
#'   `redcap_repeat_instance`, and `drugs_startdt_int_1`.
#'
#' @returns `dat` with a `regimen_number` column added.
#' @export
#'
#' @examples
#' # derive_regimen_number(reg)
derive_regimen_number <- function(dat) {
  dat |>
    dplyr::group_by(record_id) |>
    dplyr::mutate(
      regimen_number = dplyr::dense_rank(
        dplyr::pick(drugs_startdt_int_1, redcap_repeat_instance)
      )
    ) |>
    dplyr::ungroup()
}
