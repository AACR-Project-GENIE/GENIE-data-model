#' Map ca_dmets_yn codes to text labels
#'
#' @param x A numeric (or character) vector of `ca_dmets_yn` codes.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_ca_dmets_yn(c(1, 0, 10))
map_ca_dmets_yn <- function(x) {
  ca_dmets_yn_labels <- c(
    "1" = "Yes",
    "0" = "No",
    "10" = "Unknown or Not mentioned"
  )

  dplyr::recode(as.character(x), !!!ca_dmets_yn_labels)
}
