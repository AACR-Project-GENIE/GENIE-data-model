#' Map drugs_firstinst codes to text labels
#'
#' @param x A numeric (or character) vector of `drugs_firstinst` codes.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_drugs_firstinst(c(1, 2))
map_drugs_firstinst <- function(x) {
  drugs_firstinst_labels <- c(
    "1" = "Internal institution",
    "2" = "External institution"
  )

  dplyr::recode(as.character(x), !!!drugs_firstinst_labels)
}
