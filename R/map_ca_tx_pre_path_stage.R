#' Map ca_tx_pre_path_stage codes to text labels
#'
#' @param x A numeric (or character) vector of `ca_tx_pre_path_stage` codes.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_ca_tx_pre_path_stage(c(1, 0, 98, 99))
map_ca_tx_pre_path_stage <- function(x) {
  ca_tx_pre_path_stage_labels <- c(
    "1" = "Yes",
    "0" = "No",
    "98" = "Not Applicable",
    "99" = "Unknown"
  )

  dplyr::recode(as.character(x), !!!ca_tx_pre_path_stage_labels)
}
