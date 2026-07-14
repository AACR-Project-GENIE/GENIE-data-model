#' Map ca_stage_iv codes to text labels
#'
#' @param x A numeric (or character) vector of `ca_stage_iv` codes.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_ca_stage_iv(c(0, 1, 2, 9))
map_ca_stage_iv <- function(x) {
  ca_stage_iv_labels <- c(
    "0" = "No",
    "1" = "Yes",
    "2" = "Not Applicable",
    "9" = "Unknown"
  )

  dplyr::recode(as.character(x), !!!ca_stage_iv_labels)
}
