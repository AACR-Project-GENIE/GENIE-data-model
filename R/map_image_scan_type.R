#' Map image_scan_type codes to text labels
#'
#' @param x A numeric (or character) vector of `image_scan_type` codes.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_image_scan_type(c(1, 3, 5))
map_image_scan_type <- function(x) {
  image_scan_type_labels <- c(
    "1" = "CT",
    "3" = "MRI",
    "5" = "PET or PET-CT",
    "13" = "PET MRI",
    "7" = "Bone Scan",
    "9" = "Other Nuclear Medicine Scan",
    "11" = "Mammogram - Use for Breast Cancer only",
    "20" = "Other CA-specific scan"
  )

  dplyr::recode(as.character(x), !!!image_scan_type_labels)
}
