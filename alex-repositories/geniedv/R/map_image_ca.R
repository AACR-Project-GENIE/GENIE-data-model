#' Map image_ca codes to text labels
#'
#' @param x A numeric (or character) vector of `image_ca` codes.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_image_ca(c(1, 2, 3, 4))
map_image_ca <- function(x) {
  image_ca_labels <- c(
    "1" = "Yes, the Impression states or implies there is evidence of cancer",
    "2" = "No, the Impression states or implies there is no evidence of cancer",
    "3" = "The Impression is uncertain, indeterminate, or equivocal",
    "4" = "The Impression does not mention cancer"
  )

  dplyr::recode(as.character(x), !!!image_ca_labels)
}
