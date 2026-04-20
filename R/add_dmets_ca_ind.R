#' Derive all distant metastasis variables and attach to index cancer
#'
#' Orchestrates the full distant mets pipeline: imaging scans (stream A),
#' pathology reports (stream B), at-diagnosis mets (stream C), then
#' combines across streams and adds overall Stage I-III flags. The result
#' is joined onto `tables$ca_ind`.
#'
#' @param tables A named list of derived data frames, expected to contain
#'   `ca_ind`, `img`, and `path`.
#' @param cohort_ca_types Character vector of `path_ca_type` values that
#'   correspond to the cohort's cancer type (e.g.
#'   `c("Non Small Cell Lung Cancer", "Lung Cancer, NOS")` for NSCLC).
#' @param only_full_dmet_scans If `TRUE`, only include imaging scans where
#'   every site is classified as Distant (via [derive_scan_dmets_long_2()]).
#'   If `FALSE` (default), include all scans with any Distant site (via
#'   [derive_scan_dmets_long()]).
#'
#' @returns The `tables` list with `ca_ind` updated to include `dmets_*`,
#'   `dx_to_dmets_*_days`, `dmets_stage_i_iii`, and `dx_to_dmets_days`.
#' @export
#'
#' @examples
#' # tables <- add_dmets_ca_ind(tables, c("Non Small Cell Lung Cancer", "Lung Cancer, NOS"))
add_dmets_ca_ind <- function(tables, cohort_ca_types, only_full_dmet_scans = FALSE) {
  ca_ind <- tables$ca_ind
  img <- tables$img
  path <- tables$path

  scan_long <- if (only_full_dmet_scans) {
    derive_scan_dmets_long_2(img, ca_ind)
  } else {
    derive_scan_dmets_long(img, ca_ind)
  }
  scan_first <- derive_scan_dmets_first(scan_long)

  path_long <- derive_path_dmets_long(path, ca_ind, cohort_ca_types)
  path_first <- derive_path_dmets_first(path_long)

  dx_long <- derive_dx_dmets_long(ca_ind)
  dx_first <- derive_dx_dmets_first(dx_long)

  dmets_combined <- combine_dmets_derivations(
    scan_first, path_first, dx_first, ca_ind
  )

  dmets_final <- add_overall_dmets_vars(dmets_combined, ca_ind)

  tables$ca_ind <- ca_ind |>
    dplyr::left_join(dmets_final, by = c("record_id", "ca_seq"))

  tables
}
