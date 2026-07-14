#' Map ca_clin_group_stage codes to labels
#'
#' @param x A vector of ca_clin_group_stage codes.
#'
#' @returns A character vector of mapped labels.
#' @export
#'
#' @examples
#' # map_ca_clin_group_stage(ca_dx$ca_clin_group_stage)
map_ca_clin_group_stage <- function(x) {
  dplyr::recode(
    x,
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
}
