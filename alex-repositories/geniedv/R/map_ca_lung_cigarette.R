#' Map ca_lung_cigarette codes to text labels
#'
#' @param x A numeric (or character) vector of ca_lung_cigarette codes.
#'
#' @returns A character vector of cigarette use status labels.
#' @export
#'
#' @examples
#' map_ca_lung_cigarette(c(1, 2, 3, 4, 5, 100))
map_ca_lung_cigarette <- function(x) {
  cigarette_labels <- c(
    "1" = "Never used",
    "2" = "Current user",
    "3" = "Former user (quit < 1 year)",
    "4" = "Former user (quit >1 year)",
    "5" = "Former user (unknown time)",
    "100" = "Unknown"
  )

  dplyr::recode(as.character(x), !!!cigarette_labels)
}
