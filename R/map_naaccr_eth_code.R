#' Map NAACCR ethnicity codes to text labels
#'
#' @param x A numeric (or character) vector of NAACCR ethnicity codes.
#'
#' @returns A character vector of ethnicity labels.
#' @export
map_naaccr_eth_code <- function(x) {
  eth_labels <- c(
    "0" = "Non-Spanish; non-Hispanic",
    "1" = "Mexican (includes Chicano)",
    "2" = "Puerto Rican",
    "3" = "Cuban",
    "4" = "South or Central American (except Brazil)",
    "5" = "Other specified Spanish/Hispanic origin (includes European; excludes Dominican Republic)",
    "6" = "Spanish NOS or Hispanic NOS or Latino NOS",
    "7" = "Spanish surname only",
    "8" = "Dominican Republic",
    "9" = "Unknown whether Spanish or not"
  )

  dplyr::recode(as.character(x), !!!eth_labels)
}
