#' Map distant metastasis site codes to ICD-O-3 text labels
#'
#' Looks up numeric site codes in the package met-site mapping (loaded via
#' [load_met_site_map()]) and returns the corresponding label strings.
#'
#' @param x A numeric (or character) vector of metastasis site codes.
#'
#' @returns A character vector of ICD-O-3 site labels.
#' @export
#'
#' @examples
#' map_ca_dmets_site(c(340, 500, 809))
map_ca_dmets_site <- function(x) {
  met_site_labels <- load_met_site_map()
  dplyr::recode(as.character(x), !!!met_site_labels)
}
