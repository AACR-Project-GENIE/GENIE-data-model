#' Map path_ca codes to text labels
#'
#' @param x A numeric (or character) vector of `path_ca` codes.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_path_ca(c(0, 1, 10))
map_path_ca <- function(x) {
  path_ca_labels <- c(
    "0" = "No",
    "1" = "Yes",
    "10" = "Indeterminate/uncertain"
  )

  dplyr::recode(as.character(x), !!!path_ca_labels)
}
