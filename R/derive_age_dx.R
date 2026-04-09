#' Derive age at diagnosis
#'
#' Prefers curated `ca_age`, falls back to `naaccr_diagnosis_age`.
#'
#' @param dat A data frame containing `ca_age` and `naaccr_diagnosis_age`.
#'
#' @returns `dat` with an `age_dx` column added.
#' @export
#'
#' @examples
#' # derive_age_dx(ca_dx)
derive_age_dx <- function(dat) {
  dat |>
    dplyr::mutate(
      age_dx = dplyr::case_when(
        !is.na(ca_age) ~ ca_age,
        !is.na(naaccr_diagnosis_age) ~ naaccr_diagnosis_age
      )
    )
}
