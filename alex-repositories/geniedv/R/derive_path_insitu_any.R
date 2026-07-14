#' Derive path_insitu_any (any specimen with in situ disease)
#'
#' Returns `"Yes"` if any `path_insitu*` column is `"Yes"`, otherwise `"No"`.
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
      path_insitu_any = dplyr::if_else(
        any(dplyr::c_across(dplyr::all_of(insitu_cols)) == "Yes", na.rm = TRUE),
        "Yes",
        "No"
      )
    ) |>
    dplyr::ungroup()
}
