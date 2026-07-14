#' Map ca_clin_t4_det codes to text labels
#'
#' @param x A numeric (or character) vector of ca_clin_t4_det codes.
#'
#' @returns A character vector of clinical T4 detail labels.
#' @export
#'
#' @examples
#' map_ca_clin_t4_det(c(30, 33, 99))
map_ca_clin_t4_det <- function(x) {
  clin_t4_det_labels <- c(
    "30" = "T4a",
    "31" = "T4b",
    "32" = "T4c",
    "33" = "T4d",
    "34" = "T4e",
    "98" = "Not Applicable",
    "99" = "Unknown"
  )

  dplyr::recode(as.character(x), !!!clin_t4_det_labels)
}
