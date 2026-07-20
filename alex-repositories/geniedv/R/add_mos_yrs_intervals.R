#' Convert days to months
#'
#' @param x Numeric vector of days.
#' @param days_per_mo Conversion constant. Default 30.4 (from OLD_CODE).
#'
#' @returns Numeric vector of months.
#' @export
days_to_mos <- function(x, days_per_mo = 30.4) {
  x / days_per_mo
}

#' Convert days to years
#'
#' @param x Numeric vector of days.
#' @param days_per_yr Conversion constant. Default 365.25 (from OLD_CODE).
#'
#' @returns Numeric vector of years.
#' @export
days_to_yrs <- function(x, days_per_yr = 365.25) {
  x / days_per_yr
}

#' Add months and years conversions for days columns
#'
#' For each column in `cols`, adds `<stub>_mos` and `<stub>_yrs` columns
#' derived by dividing the `<stub>_days` column by the appropriate constant.
#' By default operates on every column ending in `_days`. If either of the
#' `<stub>_mos` or `<stub>_yrs` columns already exists in `dat`, that column
#' is skipped (with a message) to avoid clobbering existing data.
#'
#' @param dat A data frame.
#' @param cols Character vector of column names ending in `_days`. If `NULL`
#'   (the default), uses every column in `dat` ending in `_days`.
#' @param days_per_mo Conversion constant for days to months. Default 30.4.
#' @param days_per_yr Conversion constant for days to years. Default 365.25.
#'
#' @returns `dat` with `<stub>_mos` and `<stub>_yrs` columns added.
#' @export
#'
#' @examples
#' # add_mos_yrs_intervals(ca_dx)
#' # add_mos_yrs_intervals(ca_dx, cols = "dob_ca_dx_days")
add_mos_yrs_intervals <- function(
  dat,
  cols = NULL,
  days_per_mo = 30.4,
  days_per_yr = 365.25
) {
  if (is.null(cols)) {
    cols <- grep("_days$", colnames(dat), value = TRUE)
  }

  missing_cols <- setdiff(cols, colnames(dat))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Columns not found in {.arg dat}: {.val {missing_cols}}"
    )
  }

  for (col in cols) {
    stub <- sub("_days$", "", col)
    mos_col <- paste0(stub, "_mos")
    yrs_col <- paste0(stub, "_yrs")

    if (mos_col %in% colnames(dat) || yrs_col %in% colnames(dat)) {
      cli::cli_alert_info(
        "Skipping {.val {col}}: {.val {mos_col}} or {.val {yrs_col}} already exists in {.arg dat}."
      )
      next
    }

    dat[[mos_col]] <- days_to_mos(dat[[col]], days_per_mo)
    dat[[yrs_col]] <- days_to_yrs(dat[[col]], days_per_yr)
  }

  dat
}
