#' Fill missing `ca_stage` values using `ca_stage_iv`
#'
#' Overwrites `ca_stage` with `"IV"` whenever it is missing (`NA`,
#' `"Not Applicable"`, or `"Unknown"`) and `ca_stage_iv` is `"Yes"`.
#' Expects both columns to have already been mapped to label strings via
#' [map_ca_stage()] and [map_ca_stage_iv()].
#'
#' @param dat A data frame containing `ca_stage` and `ca_stage_iv` columns.
#'
#' @returns The input data frame with `ca_stage` updated.
#' @export
fill_ca_stage <- function(dat) {
  dat |>
    dplyr::mutate(
      ca_stage = dplyr::if_else(
        ca_stage_iv %in% "Yes" &
          (is.na(ca_stage) | ca_stage %in% c("Not Applicable", "Unknown")),
        "IV",
        ca_stage
      )
    )
}
