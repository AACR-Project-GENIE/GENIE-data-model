#' Map ca_clin_t1_det codes to text labels
#'
#' @param x A numeric (or character) vector of ca_clin_t1_det codes.
#'
#' @returns A character vector of clinical T1 detail labels.
#' @export
#'
#' @examples
#' map_ca_clin_t1_det(c(7, 9, 15, 99))
map_ca_clin_t1_det <- function(x) {
  clin_t1_det_labels <- c(
    "7" = "T1mic",
    "9" = "T1a",
    "10" = "T1a1",
    "11" = "T1a2",
    "12" = "T1b",
    "13" = "T1b1",
    "14" = "T1b2",
    "15" = "T1c",
    "16" = "T1d",
    "98" = "Not Applicable",
    "99" = "Unknown"
  )

  dplyr::recode(as.character(x), !!!clin_t1_det_labels)
}
