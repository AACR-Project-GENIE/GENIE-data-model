#' Replace checkbox values in image_scansite columns with site labels
#'
#' REDCap checkbox columns contain raw values like "Checked"/"Unchecked" or
#' 1/0. This function replaces all non-missing, non-zero, non-"Unchecked"
#' values with the corresponding site label and sets everything else to `NA`.
#'
#' @details
#' The data dictionary defines these as standard checkbox fields, so this
#' mapping does not follow directly from the dictionary. However, this matches
#' how the data were processed previously.
#'
#' @param dat A data frame containing `image_scansite___*` columns.
#'
#' @returns `dat` with scan site columns repaired.
#' @export
#'
#' @examples
#' # repair_scan_sites(img)
repair_scan_sites <- function(dat) {
  site_labels <- c(
    "image_scansite___1" = "Brain/Head",
    "image_scansite___2" = "Spine",
    "image_scansite___3" = "Neck",
    "image_scansite___4" = "Chest",
    "image_scansite___5" = "Abdomen",
    "image_scansite___6" = "Pelvis",
    "image_scansite___7" = "Extremity",
    "image_scansite___8" = "Full body"
  )

  for (col in names(site_labels)) {
    if (col %in% names(dat)) {
      dat[[col]] <- dplyr::if_else(
        !is.na(dat[[col]]) & dat[[col]] != "Unchecked" & dat[[col]] != "0",
        site_labels[[col]],
        NA_character_
      )
    }
  }

  dat
}
