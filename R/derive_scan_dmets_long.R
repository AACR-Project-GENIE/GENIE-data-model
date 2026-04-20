#' Identify distant metastases from imaging scans
#'
#' Filters to post-diagnosis scans indicating cancer, pivots `image_casite*`
#' columns long, classifies each site using [load_nsclc_met_site_groups()], and
#' keeps only distant metastases. Left-joined back to the index cancer so every
#' patient gets at least one row. A patient may have multiple rows per
#' `mets_site_group` if distant mets at the same site were observed on different
#' scans — downstream consumers should reduce to the earliest `image_scan_int`
#' per site group as needed.
#'
#' @param img Imaging data with mapped `image_casite*` columns (after
#'   [build_dv_tab_img()] has been applied).
#' @param ca_ind Index cancer subset of the cancer diagnosis form. Must contain
#'   `record_id`, `ca_seq`, `ca_cadx_int`, and `stage_dx`.
#' @param include_indeterminate If `TRUE`, include sites with `NA`
#'   classification (Indeterminate) alongside Distant. Default `FALSE`.
#'
#' @returns A long data frame with potentially multiple rows per
#'   patient-cancer-site group, including columns `mets_site_group`,
#'   `classification`, and `image_scan_int`.
#' @export
#'
#' @examples
#' # derive_scan_dmets_long(img, ca_ind)
derive_scan_dmets_long <- function(img, ca_ind, include_indeterminate = FALSE) {
  met_groups <- load_nsclc_met_site_groups()

  casite_cols <- grep("^image_casite[0-9]+$", names(img), value = TRUE)

  scans_long <- ca_ind |>
    dplyr::select(record_id, ca_seq, ca_cadx_int, stage_dx) |>
    dplyr::left_join(
      img |>
        dplyr::select(
          record_id,
          redcap_repeat_instance,
          scan_number,
          image_scan_int,
          image_ca,
          dplyr::all_of(casite_cols)
        ),
      by = "record_id"
    ) |>
    dplyr::filter(
      image_scan_int > ca_cadx_int,
      image_ca ==
        "Yes, the Impression states or implies there is evidence of cancer"
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(casite_cols),
      names_to = "casite_var",
      values_to = "casite_label",
      values_drop_na = TRUE
    ) |>
    dplyr::left_join(
      met_groups |>
        dplyr::select(icdo3_site, classification, mets_site_group),
      by = c("casite_label" = "icdo3_site")
    ) |>
    dplyr::filter(
      classification == "Distant" |
        (include_indeterminate & is.na(classification))
    )

  ca_ind |>
    dplyr::select(record_id, ca_seq, ca_cadx_int, stage_dx) |>
    dplyr::left_join(
      scans_long,
      by = c("record_id", "ca_seq", "ca_cadx_int", "stage_dx")
    )
}
