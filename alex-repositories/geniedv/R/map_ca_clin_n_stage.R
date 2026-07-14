#' Map ca_clin_n_stage codes to text labels
#'
#' @param x A numeric (or character) vector of `ca_clin_n_stage` codes.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_ca_clin_n_stage(c(1, 2, 5, 9, 13, 17, 18, 98, 99))
map_ca_clin_n_stage <- function(x) {
  ca_clin_n_stage_labels <- c(
    "1" = "NX",
    "2" = "N0",
    "5" = "N1",
    "9" = "N2",
    "13" = "N3",
    "17" = "N4",
    "18" = "N+",
    "98" = "Not Applicable",
    "99" = "Unknown"
  )

  dplyr::recode(as.character(x), !!!ca_clin_n_stage_labels)
}
