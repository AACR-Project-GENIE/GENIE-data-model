#' Map ca_clin_t3_det codes to text labels
#'
#' @param x A numeric (or character) vector of ca_clin_t3_det codes.
#'
#' @returns A character vector of clinical T3 detail labels.
#' @export
#'
#' @examples
#' map_ca_clin_t3_det(c(25, 28, 99))
map_ca_clin_t3_det <- function(x) {
  clin_t3_det_labels <- c(
    "25" = "T3a",
    "26" = "T3b",
    "27" = "T3c",
    "28" = "T3d",
    "29" = "T3e",
    "98" = "Not Applicable",
    "99" = "Unknown"
  )

  dplyr::recode(as.character(x), !!!clin_t3_det_labels)
}
