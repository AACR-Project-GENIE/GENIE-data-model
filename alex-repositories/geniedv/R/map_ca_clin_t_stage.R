#' Map ca_clin_t_stage codes to text labels
#'
#' @param x A numeric (or character) vector of ca_clin_t_stage codes.
#'
#' @returns A character vector of clinical T stage labels.
#' @export
#'
#' @examples
#' map_ca_clin_t_stage(c(1, 8, 40, 99))
map_ca_clin_t_stage <- function(x) {
  clin_t_stage_labels <- c(
    "1" = "TX",
    "2" = "T0",
    "36" = "Ta",
    "40" = "Tis",
    "8" = "T1",
    "17" = "T2",
    "24" = "T3",
    "29" = "T4",
    "98" = "Not Applicable",
    "99" = "Unknown"
  )

  dplyr::recode(as.character(x), !!!clin_t_stage_labels)
}
