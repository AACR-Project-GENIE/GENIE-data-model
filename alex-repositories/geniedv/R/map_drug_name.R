#' Map drug codes to drug names
#'
#' Looks up numeric drug codes in the package drug-name mapping (loaded via
#' [load_drug_name_map()]) and returns the corresponding drug name strings.
#'
#' @param x A numeric (or character) vector of drug codes.
#' @param investigational_num A drug code that is missing from the package
#'   drug-name mapping and should be added as `"Investigational Drug"` before
#'   recoding. Defaults to `49135`. Pass `NULL` to skip adding anything (the
#'   zero-length index assignment is a no-op).
#'
#' @returns A character vector of drug names.
#' @export
#'
#' @examples
#' map_drug_name(c(195, 376, 1000000))
map_drug_name <- function(x, investigational_num = 49135) {
  drug_name_labels <- load_drug_name_map()
  drug_name_labels[as.character(investigational_num)] <- "Investigational Drug"
  dplyr::recode(as.character(x), !!!drug_name_labels)
}
