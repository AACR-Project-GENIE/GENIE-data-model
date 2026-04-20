#' Identify distant metastases from pathology reports
#'
#' Pivots `path_ca_type*`, `path_ca*`, and `path_site*` columns long by
#' specimen number, filters to specimens with invasive cancer matching the
#' cohort, classifies each site using [load_nsclc_met_site_groups()], and
#' keeps only distant metastases. Left-joined back to the index cancer so
#' every patient gets at least one row. A patient may have multiple rows per
#' `mets_site_group` if distant mets at the same site were observed on
#' different path reports — downstream consumers should reduce to the earliest
#' `path_proc_int` per site group as needed.
#'
#' @param path Pathology data with mapped columns (after [build_dv_tab_path()]
#'   has been applied). Must contain `path_ca*`, `path_ca_type*`, and
#'   `path_site*` columns.
#' @param ca_ind Index cancer subset of the cancer diagnosis form. Must contain
#'   `record_id`, `ca_seq`, `ca_cadx_int`, and `stage_dx`.
#' @param cohort_ca_types Character vector of `path_ca_type` values that
#'   correspond to the cohort's cancer type (e.g.
#'   `c("Non Small Cell Lung Cancer", "Lung Cancer, NOS")` for NSCLC).
#' @param include_indeterminate If `TRUE`, include sites with `NA`
#'   classification (Indeterminate) alongside Distant. Default `FALSE`.
#'
#' @returns A long data frame with potentially multiple rows per
#'   patient-cancer-site group, including columns `mets_site_group`,
#'   `classification`, and `path_proc_int`.
#' @export
#'
#' @examples
#' # derive_path_dmets_long(path, ca_ind, c("Non Small Cell Lung Cancer", "Lung Cancer, NOS"))
derive_path_dmets_long <- function(path, ca_ind, cohort_ca_types,
                                   include_indeterminate = FALSE) {
  met_groups <- load_nsclc_met_site_groups()

  join_cols <- c(
    "record_id",
    "redcap_repeat_instance",
    "path_proc_number",
    "path_proc_int",
    "path_rep_number"
  )

  ca_type_cols <- grep("^path_ca_type[0-9]+$", names(path), value = TRUE)
  ca_flag_cols <- grep("^path_ca[0-9]+$", names(path), value = TRUE)
  site_cols <- grep("^path_site[0-9]+$", names(path), value = TRUE)

  path_sub <- path |>
    dplyr::select(
      dplyr::all_of(join_cols),
      dplyr::all_of(ca_type_cols),
      dplyr::all_of(ca_flag_cols),
      dplyr::all_of(site_cols)
    )

  spec_site <- path_sub |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(ca_type_cols),
      names_to = "specimen_number",
      names_prefix = "path_ca_type",
      values_to = "specimen_site",
      values_drop_na = FALSE
    ) |>
    dplyr::select(dplyr::all_of(join_cols), specimen_number, specimen_site)

  ca_flag <- path_sub |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(ca_flag_cols),
      names_to = "specimen_number",
      names_prefix = "path_ca",
      values_to = "path_ca_flag",
      values_drop_na = FALSE
    ) |>
    dplyr::select(dplyr::all_of(join_cols), specimen_number, path_ca_flag)

  site_icdo3 <- path_sub |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(site_cols),
      names_to = "specimen_number",
      names_prefix = "path_site",
      values_to = "path_site_label",
      values_drop_na = FALSE
    ) |>
    dplyr::select(dplyr::all_of(join_cols), specimen_number, path_site_label)

  path_long <- spec_site |>
    dplyr::left_join(ca_flag, by = c(join_cols, "specimen_number")) |>
    dplyr::left_join(site_icdo3, by = c(join_cols, "specimen_number")) |>
    dplyr::filter(
      path_ca_flag == "Yes",
      specimen_site %in% cohort_ca_types
    ) |>
    dplyr::left_join(
      met_groups |>
        dplyr::select(icdo3_site, classification, mets_site_group),
      by = c("path_site_label" = "icdo3_site")
    ) |>
    dplyr::filter(
      classification == "Distant" | (include_indeterminate & is.na(classification))
    )

  path_mets <- ca_ind |>
    dplyr::select(record_id, ca_seq, ca_cadx_int, stage_dx) |>
    dplyr::left_join(path_long, by = "record_id") |>
    dplyr::filter(is.na(path_proc_int) | path_proc_int > ca_cadx_int)

  ca_ind |>
    dplyr::select(record_id, ca_seq, ca_cadx_int, stage_dx) |>
    dplyr::left_join(
      path_mets |> dplyr::filter(!is.na(mets_site_group)),
      by = c("record_id", "ca_seq", "ca_cadx_int", "stage_dx")
    )
}
