#' Map hybrid death source codes to text labels
#'
#' @param x A numeric (or character) vector of hybrid death source codes.
#'
#' @returns A character vector of death source labels.
#' @export
#'
#' @examples
#' map_hybrid_death_source(c(1, 2, 3, 4, 5))
map_hybrid_death_source <- function(x) {
  death_source_labels <- c(
    "1" = "Curated",
    "2" = "EHR",
    "3" = "NDI",
    "4" = "Other",
    "5" = "Tumor Registry"
  )

  dplyr::recode(as.character(x), !!!death_source_labels)
}
