#' Derive first distant metastasis presence and timing from pathology reports
#'
#' Takes the long path-level distant mets data from
#' [derive_path_dmets_long()] and reduces to one row per patient-cancer,
#' with wide columns for presence (0/1) and timing (DOB-based
#' `path_proc_int`) of the earliest distant met per site group.
#'
#' @param path_dmets_long Output of [derive_path_dmets_long()].
#'
#' @returns A data frame with one row per `record_id`/`ca_seq`, containing:
#'   - `distant_mets_path_<site>`: 0/1 presence flag per site group
#'   - `tt_distant_mets_path_<site>`: `path_proc_int` of earliest distant
#'     met per site group (DOB-based interval)
#' @export
#'
#' @examples
#' # derive_path_dmets_first(path_dmets_long)
derive_path_dmets_first <- function(path_dmets_long) {
  earliest <- path_dmets_long |>
    dplyr::filter(!is.na(mets_site_group)) |>
    dplyr::group_by(record_id, ca_seq, mets_site_group) |>
    dplyr::slice_min(path_proc_int, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

  presence <- earliest |>
    dplyr::mutate(mets_in_group = 1L) |>
    tidyr::pivot_wider(
      id_cols = c(record_id, ca_seq),
      names_from = mets_site_group,
      names_prefix = "distant_mets_path_",
      values_from = mets_in_group,
      values_fill = 0L
    ) |>
    janitor::clean_names()

  timing <- earliest |>
    tidyr::pivot_wider(
      id_cols = c(record_id, ca_seq),
      names_from = mets_site_group,
      names_prefix = "tt_distant_mets_path_",
      values_from = path_proc_int
    ) |>
    janitor::clean_names()

  patients <- path_dmets_long |>
    dplyr::distinct(record_id, ca_seq)

  patients |>
    dplyr::left_join(presence, by = c("record_id", "ca_seq")) |>
    dplyr::left_join(timing, by = c("record_id", "ca_seq")) |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("distant_mets_path_"), \(x) tidyr::replace_na(x, 0L))
    )
}
