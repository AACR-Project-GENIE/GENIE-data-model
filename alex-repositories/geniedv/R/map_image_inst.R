#' Map image_inst codes to text labels
#'
#' @param x A numeric (or character) vector of `image_inst` codes.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_image_inst(c(1, 2))
map_image_inst <- function(x) {
  image_inst_labels <- c(
    "1" = "Internal institution",
    "2" = "External institution"
  )

  dplyr::recode(as.character(x), !!!image_inst_labels)
}
