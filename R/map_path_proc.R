#' Map path_proc codes to text labels
#'
#' @param x A numeric (or character) vector of `path_proc` codes.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_path_proc(c(1, 2, 3, 9))
map_path_proc <- function(x) {
  path_proc_labels <- c(
    "1" = "Biopsy",
    "2" = "Surgical excision",
    "3" = "Other",
    "9" = "Unknown"
  )

  dplyr::recode(as.character(x), !!!path_proc_labels)
}
