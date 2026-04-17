#' Map image_casite codes to ICD-O-3 site labels
#'
#' Uses the metastasis site mapping from [load_met_site_map()] to recode
#' `image_casite` variables.
#'
#' @param x A numeric (or character) vector of site codes.
#' @param whitespace_match_legacy If `TRUE`, use four spaces between the ICD
#'   code and description to match the legacy data format. Default `FALSE`.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_image_casite(c(349, 809))
map_image_casite <- function(x, whitespace_match_legacy = TRUE) {
  labels <- load_met_site_map()
  if (whitespace_match_legacy) {
    labels <- sub(" ", "    ", labels)
  }
  dplyr::recode(as.character(x), !!!labels)
}
