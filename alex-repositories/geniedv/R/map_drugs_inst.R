#' Map drugs_inst codes to text labels
#'
#' @param x A numeric (or character) vector of `drugs_inst` codes.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_drugs_inst(c(1, 2, 3))
map_drugs_inst <- function(x) {
  drugs_inst_labels <- c(
    "1" = "At the internal/native institution only",
    "2" = "Split across internal and external institution",
    "3" = "At external institution only"
  )

  dplyr::recode(as.character(x), !!!drugs_inst_labels)
}
