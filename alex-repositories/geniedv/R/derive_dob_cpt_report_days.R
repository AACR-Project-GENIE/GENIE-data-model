#' Derive dob_cpt_report_days
#'
#' Computes the CPT report date as days from DOB. UHN uses
#' `age_at_seq_report` while other institutions use `cpt_report_int`.
#'
#' @param dat A CPT-level data frame with columns `institution`,
#'   `age_at_seq_report`, and `cpt_report_int`.
#'
#' @returns `dat` with `dob_cpt_report_days` added.
#' @export
#'
#' @examples
#' # derive_dob_cpt_report_days(cpt)
derive_dob_cpt_report_days <- function(dat) {
  dat |>
    dplyr::mutate(
      dob_cpt_report_days = dplyr::case_when(
        institution %in% "UHN" ~ as.numeric(age_at_seq_report),
        .default = as.numeric(cpt_report_int)
      )
    )
}
