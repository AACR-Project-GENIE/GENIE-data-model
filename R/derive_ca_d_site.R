#' Derive cancer diagnosis site (ICD-O-3)
#'
#' Prefers curated `ca_site` (first word), falls back to `naaccr_site_cd`
#' reformatted as ICD-O-3 (e.g. "C340" becomes "C34.0").
#'
#' @param dat A data frame containing `ca_site` and `naaccr_site_cd`.
#'
#' @returns `dat` with a `ca_d_site` column added.
#' @export
#'
#' @examples
#' # derive_ca_d_site(ca_dx)
derive_ca_d_site <- function(dat) {
  dat |>
    dplyr::mutate(
      ca_d_site = dplyr::case_when(
        !is.na(ca_site) ~ stringr::word(ca_site, 1),
        !is.na(naaccr_site_cd) & substr(naaccr_site_cd, 1, 1) == "C" ~
          paste0(substr(naaccr_site_cd, 1, 3), ".", substr(naaccr_site_cd, 4, 4)),
        !is.na(naaccr_site_cd) ~ naaccr_site_cd
      )
    )
}
