#' Identify distant metastases from imaging scans (all-distant-sites only)
#'
#' Similar to [derive_scan_dmets_long()], but only includes scans where
#' every site is classified as Distant. Scans covering any Local/Regional
#' site are excluded because the cancer finding cannot be attributed to a
#' specific site.
#'
#' @inheritParams derive_scan_dmets_long
#'
#' @returns A long data frame with potentially multiple rows per
#'   patient-cancer-site group, including columns `mets_site_group`,
#'   `classification`, and `image_scan_int`.
#' @export
#'
#' @examples
#' # derive_scan_dmets_long_2(img, ca_ind)
derive_scan_dmets_long_2 <- function(
  img,
  ca_ind,
  include_indeterminate = FALSE
) {
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
    )

  scan_keys <- c(
    "record_id",
    "ca_seq",
    "redcap_repeat_instance",
    "image_scan_int"
  )

  all_distant_scans <- scans_long |>
    dplyr::group_by(dplyr::across(dplyr::all_of(scan_keys))) |>
    dplyr::filter(all(
      classification == "Distant" |
        (include_indeterminate & is.na(classification)),
      na.rm = !include_indeterminate
    )) |>
    dplyr::ungroup()

  ca_ind |>
    dplyr::select(record_id, ca_seq, ca_cadx_int, stage_dx) |>
    dplyr::left_join(
      all_distant_scans,
      by = c("record_id", "ca_seq", "ca_cadx_int", "stage_dx")
    )
}
