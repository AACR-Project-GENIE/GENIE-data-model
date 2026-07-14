#' Map ca_path_group_stage codes to text labels
#'
#' @param x A numeric (or character) vector of `ca_path_group_stage` codes.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_ca_path_group_stage(c(0, 3, 24, 98, 99))
map_ca_path_group_stage <- function(x) {
  ca_path_group_stage_labels <- c(
    "0" = "0",
    "1" = "0A",
    "2" = "0is",
    "3" = "I",
    "4" = "IA",
    "5" = "IA1",
    "6" = "IA2",
    "31" = "IA3",
    "7" = "IB",
    "8" = "IB1",
    "9" = "IB2",
    "10" = "IC",
    "11" = "IS",
    "12" = "II",
    "13" = "IIA",
    "14" = "IIA1",
    "15" = "IIA2",
    "16" = "IIB",
    "17" = "IIC",
    "18" = "III",
    "19" = "IIIA",
    "32" = "IIIA1",
    "33" = "IIIA2",
    "20" = "IIIB",
    "21" = "IIIC",
    "22" = "IIIC1",
    "23" = "IIIC2",
    "24" = "IV",
    "25" = "IVA",
    "26" = "IVA1",
    "27" = "IVA2",
    "28" = "IVB",
    "29" = "IVC",
    "30" = "Occult",
    "98" = "Not Applicable",
    "99" = "Unknown"
  )

  dplyr::recode(as.character(x), !!!ca_path_group_stage_labels)
}
