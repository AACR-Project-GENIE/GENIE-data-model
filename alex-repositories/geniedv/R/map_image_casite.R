#' Map image_casite codes to ICD-O-3 site labels
#'
#' Uses the metastasis site mapping from [load_met_site_map()] to recode
#' `image_casite` variables.
#'
#' @param x A numeric (or character) vector of site codes.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_image_casite(c(349, 809))
map_image_casite <- function(x) {
  labels <- load_met_site_map()

  dplyr::recode(as.character(x), !!!labels)
}
