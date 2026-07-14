#' Derive scan_sites by concatenating non-NA image_scansite columns
#'
#' Collapses `image_scansite___1` through `image_scansite___8` into a single
#' comma-separated string per row.
#'
#' @param dat A data frame containing `image_scansite___*` columns (after
#'   [repair_scan_sites()] has been applied).
#'
#' @returns `dat` with `scan_sites` added.
#' @export
#'
#' @examples
#' # derive_scan_sites(img)
derive_scan_sites <- function(dat) {
  site_cols <- paste0("image_scansite___", 1:8)
  present <- intersect(site_cols, names(dat))

  dat |>
    tidyr::unite(
      "scan_sites",
      dplyr::all_of(present),
      sep = ", ",
      na.rm = TRUE,
      remove = FALSE
    )
}
