#' Derive dob_reg_end_all_int (end of all drugs in regimen, DOB-indexed)
#'
#' Computes the regimen "end of all drugs" date as the latest drug end date
#' across the five drug slots in a regimen, expressed as days from DOB.
#' Parallels the legacy derivation: a row-wise `pmax` over
#' `drugs_enddt_int_1..drugs_enddt_int_5` with `na.rm = TRUE`. Note that the
#' legacy gating on `drugs_dc_ynu == "Yes"` and the absence of investigational
#' drugs is already enforced upstream in [derive_reg()] by masking
#' `drugs_enddt_int_<n>` per drug slot, so no additional gating is applied
#' here.
#'
#' @param dat A regimen-level data frame with numeric columns
#'   `drugs_enddt_int_1`, `drugs_enddt_int_2`, `drugs_enddt_int_3`,
#'   `drugs_enddt_int_4`, `drugs_enddt_int_5`.
#'
#' @returns `dat` with a `dob_reg_end_all_int` column added.
#' @export
#'
#' @examples
#' # derive_reg_end_all_int(reg)
derive_reg_end_all_int <- function(dat) {
  dat |>
    dplyr::mutate(
      dob_reg_end_all_int = pmax(
        drugs_enddt_int_1,
        drugs_enddt_int_2,
        drugs_enddt_int_3,
        drugs_enddt_int_4,
        drugs_enddt_int_5,
        na.rm = TRUE
      )
    )
}
