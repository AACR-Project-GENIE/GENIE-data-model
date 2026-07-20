#' Map ca_clin_t2_det codes to text labels
#'
#' @param x A numeric (or character) vector of ca_clin_t2_det codes.
#'
#' @returns A character vector of clinical T2 detail labels.
#' @export
#'
#' @examples
#' map_ca_clin_t2_det(c(18, 21, 23, 99))
map_ca_clin_t2_det <- function(x) {
  clin_t2_det_labels <- c(
    "18" = "T2a",
    "19" = "T2a1",
    "20" = "T2a2",
    "21" = "T2b",
    "22" = "T2c",
    "23" = "T2d",
    "98" = "Not Applicable",
    "99" = "Unknown"
  )

  dplyr::recode(as.character(x), !!!clin_t2_det_labels)
}
