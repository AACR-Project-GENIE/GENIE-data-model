#' Load the metastasis site code to label mapping
#'
#' Reads the package data file `inst/extdata/met_sites.txt` and parses it
#' into a named character vector. Each line of the file is expected to be
#' `<code><whitespace><site label>`, where `<code>` is a numeric site code
#' (no internal whitespace) and `<site label>` is the rest of the line.
#'
#' @returns A named character vector whose names are the site codes (as
#'   character) and whose values are the site label strings.
#' @export
#'
#' @examples
#' head(load_met_site_map())
load_met_site_map <- function() {
  path <- system.file("extdata", "met_sites.txt", package = "geniedv")
  if (!nzchar(path)) {
    cli::cli_abort("Could not locate {.file extdata/met_sites.txt} in geniedv.")
  }

  lines <- readr::read_lines(path)
  lines <- lines[nzchar(lines)]

  matches <- stringr::str_match(lines, "^(\\S+)\\s+(.*)$")

  # Column 2 is the first capture group (the code); NA means the regex failed
  # to match, i.e. the line doesn't follow the expected "<code> <label>" format.
  bad <- is.na(matches[, 2])
  if (any(bad)) {
    cli::cli_abort(c(
      "Could not parse {sum(bad)} line{?s} in {.file met_sites.txt}.",
      i = "First offending line: {.val {lines[which(bad)[1]]}}"
    ))
  }

  stats::setNames(matches[, 3], matches[, 2])
}
