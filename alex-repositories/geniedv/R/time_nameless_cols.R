#' Trim columns with auto-gen names (starting with ...)
#'
#' @param dat A loaded dataset.
#'
#' @returns `dat` with columns starting with "..." removed.
#' @export
#'
#' @examples
trim_nameless_cols <- function(dat) {
  dat %>%
    dplyr::select(-tidyselect::matches("^\\.\\.\\."))
}
