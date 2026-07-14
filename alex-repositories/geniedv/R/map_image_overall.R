#' Map image_overall codes to text labels
#'
#' @param x A numeric (or character) vector of `image_overall` codes.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_image_overall(c(1, 2, 3, 4, 5))
map_image_overall <- function(x) {
  image_overall_labels <- c(
    "1" = "Improving/Responding",
    "2" = "Stable/No change",
    "3" = "Mixed",
    "4" = "Progressing/Worsening/Enlarging",
    "5" = "Not stated/Indeterminate"
  )

  dplyr::recode(as.character(x), !!!image_overall_labels)
}
