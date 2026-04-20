#' Identify distant metastases at diagnosis for Stage IV patients
#'
#' For Stage IV patients with `ca_dmets_yn == "Yes"`, pivots
#' `ca_first_dmets*` columns long, classifies each site using
#' [load_nsclc_met_site_groups()], and keeps only distant metastases.
#' Left-joined back to the index cancer so every patient gets at least one
#' row. The timing for at-diagnosis mets is set to `ca_cadx_int` (i.e.
#' time-to-mets = 0).
#'
#' @param ca_ind Index cancer subset of the cancer diagnosis form with mapped
#'   `ca_first_dmets*` columns (after [build_dv_tab_ca_dx()] has been
#'   applied). Must contain `record_id`, `ca_seq`, `ca_cadx_int`, `stage_dx`,
#'   `ca_dmets_yn`, and `ca_first_dmets*` columns.
#'
#' @returns A long data frame with potentially multiple rows per
#'   patient-cancer-site group, including columns `mets_site_group`,
#'   `classification`, and `ca_cadx_int`.
#' @export
#'
#' @examples
#' # derive_dx_dmets_long(ca_ind)
derive_dx_dmets_long <- function(ca_ind) {
  met_groups <- load_nsclc_met_site_groups()

  dmets_cols <- grep("^ca_first_dmets[0-9]+$", names(ca_ind), value = TRUE)

  dx_long <- ca_ind |>
    dplyr::filter(stage_dx == "Stage IV", ca_dmets_yn == "Yes") |>
    dplyr::select(
      record_id,
      ca_seq,
      ca_cadx_int,
      stage_dx,
      dplyr::all_of(dmets_cols)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(dmets_cols),
      names_to = "mets_number",
      values_to = "dmets_label",
      values_drop_na = TRUE
    ) |>
    dplyr::left_join(
      met_groups |>
        dplyr::select(icdo3_site, classification, mets_site_group),
      by = c("dmets_label" = "icdo3_site")
    ) |>
    dplyr::filter(classification == "Distant")

  ca_ind |>
    dplyr::select(record_id, ca_seq, ca_cadx_int, stage_dx) |>
    dplyr::left_join(
      dx_long,
      by = c("record_id", "ca_seq", "ca_cadx_int", "stage_dx")
    )
}
