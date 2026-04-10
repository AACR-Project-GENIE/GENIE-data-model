#' Load the drug code to drug name mapping
#'
#' Reads the package data file `inst/extdata/drug_names.txt` and parses it
#' into a named character vector. Each line of the file is expected to be
#' `<code><whitespace><drug name>`, where `<code>` is a numeric drug code
#' (no internal whitespace) and `<drug name>` is the rest of the line.
#'
#' @returns A named character vector whose names are the drug codes (as
#'   character) and whose values are the drug name strings.
#' @export
#'
#' @examples
#' head(load_drug_name_map())
load_drug_name_map <- function() {
  path <- system.file("extdata", "drug_names.txt", package = "geniedv")
  if (!nzchar(path)) {
    cli::cli_abort("Could not locate {.file extdata/drug_names.txt} in geniedv.")
  }

  lines <- readr::read_lines(path)
  lines <- lines[nzchar(lines)]

  matches <- stringr::str_match(lines, "^(\\S+)\\s+(.*)$")

  # Column 2 is the first capture group (the code); NA means the regex failed
  # to match this line, so we couldn't parse a code out of it.
  bad <- is.na(matches[, 2])
  if (any(bad)) {
    cli::cli_abort(c(
      "Could not parse {sum(bad)} line{?s} in {.file drug_names.txt}.",
      i = "First offending line: {.val {lines[which(bad)[1]]}}"
    ))
  }

  stats::setNames(matches[, 3], matches[, 2])
}
