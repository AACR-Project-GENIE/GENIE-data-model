#' Map drugs_dc_ynu codes to text labels
#'
#' @param x A numeric (or character) vector of `drugs_dc_ynu` codes.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_drugs_dc_ynu(c(1, 0, 10))
map_drugs_dc_ynu <- function(x) {
  drugs_dc_ynu_labels <- c(
    "1" = "Yes",
    "0" = "No",
    "10" = "Unknown, no documentation found"
  )

  dplyr::recode(as.character(x), !!!drugs_dc_ynu_labels)
}
