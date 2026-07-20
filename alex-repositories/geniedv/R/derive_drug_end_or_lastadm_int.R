#' Combine drug end date and last-administered date into one DOB-indexed interval
#'
#' For a single drug slot, returns the DOB-indexed day on which the drug
#' either ended (if the drug was discontinued, `drugs_dc_ynu == "Yes"`) or
#' was last administered (if `drugs_dc_ynu` is `"No"` or
#' `"Unknown, no documentation found"`). Parallels the logic of
#' `dx_drug_end_or_lastadm_int` in the legacy derivation, but stays on the
#' DOB-indexed scale (as the `drugs_*` columns do) rather than subtracting
#' the diagnosis date.
#'
#' Intended to be applied one drug slot at a time (e.g. drug 1, drug 2, ...)
#' by the caller, to produce `drugs_drug_end_or_lastadm_int_<n>` columns.
#'
#' @param drugs_enddt_int Numeric vector: days from DOB to drug end date
#'   for a single drug slot (e.g. `drugs_enddt_int_1`).
#' @param drugs_lastdt_int Numeric vector: days from DOB to drug last
#'   administered date for the same drug slot (e.g. `drugs_lastdt_int_1`).
#' @param drugs_dc_ynu Character vector of discontinuation status, already
#'   mapped to labels by [map_drugs_dc_ynu()]: one of `"Yes"`, `"No"`,
#'   `"Unknown, no documentation found"`.
#'
#' @returns A numeric vector of the combined DOB-indexed end-or-last-admin
#'   day, the same length as the inputs.
#' @export
#'
#' @examples
#' derive_drug_end_or_lastadm_int(
#'   drugs_enddt_int = c(100, NA, 200),
#'   drugs_lastdt_int = c(NA, 150, NA),
#'   drugs_dc_ynu = c("Yes", "No", "Unknown, no documentation found")
#' )
derive_drug_end_or_lastadm_int <- function(
  drugs_enddt_int,
  drugs_lastdt_int,
  drugs_dc_ynu
) {
  dplyr::case_when(
    drugs_dc_ynu %in% "Yes" ~ drugs_enddt_int,
    drugs_dc_ynu %in% c("No", "Unknown, no documentation found") ~
      drugs_lastdt_int,
    .default = NA_real_
  )
}
