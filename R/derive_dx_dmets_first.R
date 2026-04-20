#' Derive first distant metastasis presence and timing at diagnosis
#'
#' Takes the long at-diagnosis distant mets data from
#' [derive_dx_dmets_long()] and reduces to one row per patient-cancer,
#' with wide columns for presence (0/1) and timing (DOB-based
#' `ca_cadx_int`) of each site group. Since these are mets at diagnosis,
#' the timing value is always `ca_cadx_int` (time-to-mets = 0).
#'
#' @param dx_dmets_long Output of [derive_dx_dmets_long()].
#'
#' @returns A data frame with one row per `record_id`/`ca_seq`, containing:
#'   - `distant_mets_dx_<site>`: 0/1 presence flag per site group
#'   - `tt_distant_mets_dx_<site>`: `ca_cadx_int` for each site group
#'     (DOB-based interval)
#' @export
#'
#' @examples
#' # derive_dx_dmets_first(dx_dmets_long)
derive_dx_dmets_first <- function(dx_dmets_long) {
  earliest <- dx_dmets_long |>
    dplyr::filter(!is.na(mets_site_group)) |>
    dplyr::distinct(record_id, ca_seq, mets_site_group, ca_cadx_int)

  presence <- earliest |>
    dplyr::mutate(mets_in_group = 1L) |>
    tidyr::pivot_wider(
      id_cols = c(record_id, ca_seq),
      names_from = mets_site_group,
      names_prefix = "distant_mets_dx_",
      values_from = mets_in_group,
      values_fill = 0L
    ) |>
    janitor::clean_names()

  timing <- earliest |>
    tidyr::pivot_wider(
      id_cols = c(record_id, ca_seq),
      names_from = mets_site_group,
      names_prefix = "tt_distant_mets_dx_",
      values_from = ca_cadx_int
    ) |>
    janitor::clean_names()

  patients <- dx_dmets_long |>
    dplyr::distinct(record_id, ca_seq)

  patients |>
    dplyr::left_join(presence, by = c("record_id", "ca_seq")) |>
    dplyr::left_join(timing, by = c("record_id", "ca_seq")) |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("distant_mets_dx_"), \(x) {
        tidyr::replace_na(x, 0L)
      })
    )
}
