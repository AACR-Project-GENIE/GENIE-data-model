#' Remove duplicate site codes across image_casite columns
#'
#' Within each row, if the same code appears in multiple `image_casite*`
#' columns, only the first occurrence is kept and later duplicates are set
#' to `NA`.
#'
#' @param dat A data frame containing `image_casite*` columns.
#'
#' @returns `dat` with duplicate site codes removed.
#' @export
#'
#' @examples
#' # remove_duplicated_ca_site(img)
remove_duplicated_ca_site <- function(dat) {
  casite_cols <- grep("^image_casite[0-9]+$", names(dat), value = TRUE)

  if (length(casite_cols) == 0) {
    return(dat)
  }

  casite_mat <- as.matrix(dat[, casite_cols])
  for (i in seq_len(nrow(casite_mat))) {
    vals <- casite_mat[i, ]
    duped <- duplicated(vals) & !is.na(vals)
    casite_mat[i, duped] <- NA
  }
  dat[, casite_cols] <- casite_mat

  dat
}
