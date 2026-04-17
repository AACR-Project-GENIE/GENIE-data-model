#' Map path_proc_margins codes to text labels
#'
#' @param x A numeric (or character) vector of `path_proc_margins` codes.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_path_proc_margins(c(0, 1, 10))
map_path_proc_margins <- function(x) {
  path_proc_margins_labels <- c(
    "0" = "No",
    "1" = "Yes",
    "10" = "Indeterminate/uncertain"
  )

  dplyr::recode(as.character(x), !!!path_proc_margins_labels)
}
