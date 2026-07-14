#' Derive regimen_number_within_cancer
#'
#' Numbers regimens within each cancer diagnosis, ordered by overall
#' `regimen_number`. Groups by `record_id` and `redcap_ca_seq`, arranges
#' by `regimen_number`, and assigns `1, 2, ..., n`. Must be called after
#' [derive_regimen_number()].
#'
#' @param dat A regimen-level data frame with one row per
#'   regimen-associated cancer diagnosis. Must contain `record_id`,
#'   `redcap_ca_seq`, and `regimen_number`.
#'
#' @returns `dat` with a `regimen_number_within_cancer` column added.
#' @export
#'
#' @examples
#' # derive_regimen_number_within_cancer(reg)
derive_regimen_number_within_cancer <- function(dat) {
  dat |>
    dplyr::group_by(record_id, redcap_ca_seq) |>
    dplyr::mutate(
      regimen_number_within_cancer = dplyr::row_number(regimen_number)
    ) |>
    dplyr::ungroup()
}
