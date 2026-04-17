#' Derive path_insitu_any (count of specimens with in situ disease)
#'
#' Counts how many of the `path_insitu*` columns have value `"Yes"` per row.
#' Should be called after [map_path_insitu()] has been applied.
#'
#' @param dat A data frame containing mapped `path_insitu*` columns.
#'
#' @returns `dat` with `path_insitu_any` added.
#' @export
#'
#' @examples
#' # derive_path_insitu_any(path)
derive_path_insitu_any <- function(dat) {
  insitu_cols <- grep("^path_insitu[0-9]+$", names(dat), value = TRUE)

  dat |>
    dplyr::rowwise() |>
    dplyr::mutate(
      path_insitu_any = sum(dplyr::c_across(dplyr::all_of(insitu_cols)) == "Yes", na.rm = TRUE)
    ) |>
    dplyr::ungroup()
}
