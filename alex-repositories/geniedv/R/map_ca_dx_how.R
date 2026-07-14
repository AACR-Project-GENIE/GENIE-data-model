#' Map ca_dx_how codes to text labels
#'
#' @param x A numeric (or character) vector of ca_dx_how codes.
#'
#' @returns A character vector of diagnosis method labels.
#' @export
#'
#' @examples
#' map_ca_dx_how(c(1, 2, 3, 4))
map_ca_dx_how <- function(x) {
  dx_how_labels <- c(
    "1" = "Pathology",
    "2" = "Imaging",
    "3" = "Physical Exam",
    "4" = "Other"
  )

  dplyr::recode(as.character(x), !!!dx_how_labels)
}
