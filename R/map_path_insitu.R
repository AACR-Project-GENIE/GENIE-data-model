#' Map path_insitu codes to text labels
#'
#' @param x A numeric (or character) vector of `path_insitu` codes.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_path_insitu(c(0, 1, 10))
map_path_insitu <- function(x) {
  path_insitu_labels <- c(
    "0" = "No",
    "1" = "Yes",
    "10" = "Indeterminate/uncertain"
  )

  dplyr::recode(as.character(x), !!!path_insitu_labels)
}
