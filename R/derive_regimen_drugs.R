#' Derive regimen_drugs (alphabetized drug-name list per regimen)
#'
#' For each regimen row, collapses the individual `drugs_drug_<n>` drug-name
#' columns into a single comma-separated `regimen_drugs` string listing the
#' drugs in alphabetical order. Parallels the legacy derivation in
#' `create_derived_vars.Rmd`: take the short name (everything before the
#' first `(`), sort, and concatenate with `", "`. "Other" free-text drug
#' columns (`drugs_drug_oth*`) and mask columns are not included.
#'
#' @param dat A regimen-level data frame containing the wide
#'   `drugs_drug_<n>` drug-name columns. Typically the output of earlier
#'   `derive_reg()` steps, after `map_drug_name()` has replaced drug codes
#'   with names.
#'
#' @returns `dat` with a `regimen_drugs` character column added.
#' @export
#'
#' @examples
#' # derive_regimen_drugs(reg)
derive_regimen_drugs <- function(dat) {
  dat |>
    dplyr::rowwise() |>
    dplyr::mutate(
      regimen_drugs = paste(
        sort(stringr::word(
          dplyr::c_across(dplyr::matches("^drugs_drug_\\d+$")),
          sep = "\\("
        )),
        collapse = ", "
      )
    ) |>
    dplyr::ungroup()
}
