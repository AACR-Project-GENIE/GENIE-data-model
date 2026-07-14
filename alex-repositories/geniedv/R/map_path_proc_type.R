#' Map path_proc_type codes to text labels
#'
#' @param x A numeric (or character) vector of `path_proc_type` codes.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_path_proc_type(c(1, 2, 3))
map_path_proc_type <- function(x) {
  path_proc_type_labels <- c(
    "1" = "Cytology",
    "2" = "Surgical pathology",
    "4" = "Hematopathology",
    "5" = "Flow Cytometry",
    "3" = "Other"
  )

  dplyr::recode(as.character(x), !!!path_proc_type_labels)
}
