#' Map NAACCR race codes to text labels
#'
#' @param x A numeric (or character) vector of NAACCR race codes.
#'
#' @returns A character vector of race labels.
#' @export
#'
#' @examples
#' map_naaccr_race(c(1, 2, 4, 99))
map_naaccr_race <- function(x) {
  race_labels <- c(
    "1" = "White",
    "2" = "Black",
    "3" = "American Indian, Aleutian, or Eskimo",
    "4" = "Chinese",
    "5" = "Japanese",
    "6" = "Filipino",
    "7" = "Hawaiian",
    "8" = "Korean",
    "10" = "Vietnamese",
    "11" = "Laotian",
    "12" = "Hmong",
    "13" = "Kampuchean (Cambodian)",
    "14" = "Thai",
    "15" = "Asian Indian or Pakistani NOS",
    "16" = "Asian Indian",
    "17" = "Pakistani",
    "20" = "Micronesian NOS",
    "21" = "Chamorro/Chamoru",
    "22" = "Guamanian NOS",
    "25" = "Polynesian NOS",
    "26" = "Tahitian",
    "27" = "Samoan",
    "28" = "Tongan",
    "30" = "Melanesian NOS",
    "31" = "Fiji Islander",
    "32" = "New Guinean",
    "96" = "Other Asian including Asian NOS and Oriental NOS",
    "97" = "Pacific Islander NOS",
    "98" = "Other",
    "99" = "Unknown"
  )

  dplyr::recode(as.character(x), !!!race_labels)
}
