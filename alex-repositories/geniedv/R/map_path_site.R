#' Map path_site codes to ICD-O-3 site labels
#'
#' Uses the metastasis site mapping from [load_met_site_map()] to recode
#' `path_site` variables.
#'
#' @param x A numeric (or character) vector of site codes.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_path_site(c(349, 809))
map_path_site <- function(x) {
  labels <- load_met_site_map()
  dplyr::recode(as.character(x), !!!labels)
}
