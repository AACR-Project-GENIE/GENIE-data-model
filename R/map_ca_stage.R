#' Map ca_stage codes to text labels
#'
#' @param x A numeric (or character) vector of `ca_stage` codes.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_ca_stage(c(0, 1, 13, 98, 99))
map_ca_stage <- function(x) {
  ca_stage_labels <- c(
    "0" = "0",
    "17" = "0A",
    "18" = "0is",
    "1" = "I",
    "2" = "IA",
    "19" = "IA1",
    "20" = "IA2",
    "21" = "IA3",
    "3" = "IB",
    "4" = "IC",
    "5" = "II",
    "6" = "IIA",
    "7" = "IIB",
    "8" = "IIC",
    "9" = "III",
    "10" = "IIIA",
    "22" = "IIIA1",
    "23" = "IIIA2",
    "11" = "IIIB",
    "12" = "IIIC",
    "13" = "IV",
    "14" = "IVA",
    "15" = "IVB",
    "16" = "IVC",
    "98" = "Not Applicable",
    "99" = "Unknown"
  )

  dplyr::recode(as.character(x), !!!ca_stage_labels)
}
