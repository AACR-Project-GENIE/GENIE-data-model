#' Map drug codes to drug names
#'
#' Looks up numeric drug codes in the package drug-name mapping (loaded via
#' [load_drug_name_map()]) and returns the corresponding drug name strings.
#'
#' @param x A numeric (or character) vector of drug codes.
#'
#' @returns A character vector of drug names.
#' @export
#'
#' @examples
#' map_drug_name(c(195, 376, 1000000))
map_drug_name <- function(x) {
  drug_name_labels <- load_drug_name_map()
  dplyr::recode(as.character(x), !!!drug_name_labels)
}
