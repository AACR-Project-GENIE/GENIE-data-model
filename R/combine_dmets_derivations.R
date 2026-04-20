#' Pivot a per-stream dmets_first table to long format
#'
#' @param df Output of one of the `derive_*_dmets_first()` functions.
#' @param stream One of `"scan"`, `"path"`, or `"dx"`.
#'
#' @returns A long data frame with columns `record_id`, `ca_seq`, `site`,
#'   `presence`, and `timing`.
#' @noRd
pivot_stream_long <- function(df, stream) {
  presence_prefix <- paste0("distant_mets_", stream, "_")
  timing_prefix <- paste0("tt_distant_mets_", stream, "_")

  pres <- df |>
    dplyr::select(record_id, ca_seq, dplyr::starts_with(presence_prefix)) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with(presence_prefix),
      names_prefix = presence_prefix,
      names_to = "site",
      values_to = "presence"
    )

  tt <- df |>
    dplyr::select(record_id, ca_seq, dplyr::starts_with(timing_prefix)) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with(timing_prefix),
      names_prefix = timing_prefix,
      names_to = "site",
      values_to = "timing"
    )

  dplyr::left_join(pres, tt, by = c("record_id", "ca_seq", "site"))
}

#' Combine distant metastasis flags from all three streams
#'
#' Pivots the wide per-stream results from [derive_scan_dmets_first()],
#' [derive_path_dmets_first()], and [derive_dx_dmets_first()] to long
#' format, takes the max presence and earliest timing across streams per
#' site group, then pivots back wide. Timing is converted from DOB-based
#' intervals to diagnosis-relative days.
#'
#' @param scan_first Output of [derive_scan_dmets_first()].
#' @param path_first Output of [derive_path_dmets_first()].
#' @param dx_first Output of [derive_dx_dmets_first()].
#' @param ca_ind Index cancer data frame containing `record_id`, `ca_seq`,
#'   and `ca_cadx_int`.
#'
#' @returns A data frame with one row per `record_id`/`ca_seq`, containing:
#'   - `dmets_<site>`: 0/1 presence flag per site group
#'   - `dx_to_dmets_<site>_days`: days from diagnosis to first distant met
#' @export
#'
#' @examples
#' # combine_dmets_derivations(scan_first, path_first, dx_first, ca_ind)
combine_dmets_derivations <- function(
  scan_first,
  path_first,
  dx_first,
  ca_ind
) {
  long <- dplyr::bind_rows(
    pivot_stream_long(scan_first, "scan"),
    pivot_stream_long(path_first, "path"),
    pivot_stream_long(dx_first, "dx")
  )

  # suppressWarnings: all-NA groups produce Inf/-Inf, cleaned up below
  combined <- long |>
    dplyr::group_by(record_id, ca_seq, site) |>
    dplyr::summarize(
      dmets = suppressWarnings(max(presence, na.rm = TRUE)),
      tt_dmets = suppressWarnings(min(timing, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dmets = dplyr::if_else(is.infinite(dmets), 0L, as.integer(dmets)),
      tt_dmets = dplyr::if_else(is.infinite(tt_dmets), NA_real_, tt_dmets)
    )

  combined <- combined |>
    dplyr::left_join(
      ca_ind |> dplyr::distinct(record_id, ca_seq, ca_cadx_int),
      by = c("record_id", "ca_seq")
    ) |>
    dplyr::mutate(
      dx_to_dmets_days = tt_dmets - ca_cadx_int
    )

  dmets_wide <- combined |>
    tidyr::pivot_wider(
      id_cols = c(record_id, ca_seq),
      names_from = site,
      names_glue = "dmets_{site}",
      values_from = dmets,
      values_fill = 0L
    )

  days_wide <- combined |>
    tidyr::pivot_wider(
      id_cols = c(record_id, ca_seq),
      names_from = site,
      names_glue = "dx_to_dmets_{site}_days",
      values_from = dx_to_dmets_days
    )

  patients <- ca_ind |>
    dplyr::distinct(record_id, ca_seq)

  patients |>
    dplyr::left_join(dmets_wide, by = c("record_id", "ca_seq")) |>
    dplyr::left_join(days_wide, by = c("record_id", "ca_seq")) |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("dmets_"), \(x) tidyr::replace_na(x, 0L))
    )
}
