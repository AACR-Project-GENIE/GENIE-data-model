#' Map NAACCR sex codes to text labels
#'
#' @param x A numeric (or character) vector of NAACCR sex codes.
#'
#' @returns A character vector of sex labels.
#' @export
#'
#' @examples
#' map_naaccr_sex_code(c(1, 2, 9))
map_naaccr_sex_code <- function(x) {
  sex_labels <- c(
    "1" = "Male",
    "2" = "Female",
    "3" = "Other intersex, disorders of sexual development/DSD",
    "4" = "Transsexual NOS",
    "5" = "Transsexual natal male",
    "6" = "Transsexual natal female",
    "9" = "Not stated Unknown"
  )

  dplyr::recode(as.character(x), !!!sex_labels)
}
