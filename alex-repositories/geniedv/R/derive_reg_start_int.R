#' Derive dob_reg_start_int (regimen start, DOB-indexed)
#'
#' Computes the regimen start date as the earliest drug start date across
#' the five drug slots in a regimen, expressed as days from DOB. Parallels
#' the legacy derivation: a row-wise `pmin` over
#' `drugs_startdt_int_1..drugs_startdt_int_5` with `na.rm = TRUE`.
#'
#' @param dat A regimen-level data frame with numeric columns
#'   `drugs_startdt_int_1`, `drugs_startdt_int_2`, `drugs_startdt_int_3`,
#'   `drugs_startdt_int_4`, `drugs_startdt_int_5`.
#'
#' @returns `dat` with a `dob_reg_start_int` column added.
#' @export
#'
#' @examples
#' # derive_reg_start_int(reg)
derive_reg_start_int <- function(dat) {
  dat |>
    dplyr::mutate(
      dob_reg_start_int = pmin(
        drugs_startdt_int_1,
        drugs_startdt_int_2,
        drugs_startdt_int_3,
        drugs_startdt_int_4,
        drugs_startdt_int_5,
        na.rm = TRUE
      )
    )
}
