#' Derive first distant metastasis presence and timing from imaging scans
#'
#' Takes the long scan-level distant mets data from
#' [derive_scan_dmets_long()] and reduces to one row per patient-cancer,
#' with wide columns for presence (0/1) and timing (DOB-based
#' `image_scan_int`) of the earliest distant met per site group.
#'
#' @param scan_dmets_long Output of [derive_scan_dmets_long()].
#'
#' @returns A data frame with one row per `record_id`/`ca_seq`, containing:
#'   - `distant_mets_scan_<site>`: 0/1 presence flag per site group
#'   - `tt_distant_mets_scan_<site>`: `image_scan_int` of earliest distant
#'     met per site group (DOB-based interval)
#' @export
#'
#' @examples
#' # derive_scan_dmets_first(scan_dmets_long)
derive_scan_dmets_first <- function(scan_dmets_long) {
  earliest <- scan_dmets_long |>
    dplyr::filter(!is.na(mets_site_group)) |>
    dplyr::group_by(record_id, ca_seq, mets_site_group) |>
    dplyr::slice_min(image_scan_int, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

  presence <- earliest |>
    dplyr::mutate(mets_in_group = 1L) |>
    tidyr::pivot_wider(
      id_cols = c(record_id, ca_seq),
      names_from = mets_site_group,
      names_prefix = "distant_mets_scan_",
      values_from = mets_in_group,
      values_fill = 0L
    ) |>
    janitor::clean_names()

  timing <- earliest |>
    tidyr::pivot_wider(
      id_cols = c(record_id, ca_seq),
      names_from = mets_site_group,
      names_prefix = "tt_distant_mets_scan_",
      values_from = image_scan_int
    ) |>
    janitor::clean_names()

  patients <- scan_dmets_long |>
    dplyr::distinct(record_id, ca_seq)

  patients |>
    dplyr::left_join(presence, by = c("record_id", "ca_seq")) |>
    dplyr::left_join(timing, by = c("record_id", "ca_seq")) |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("distant_mets_scan_"), \(x) tidyr::replace_na(x, 0L))
    )
}
