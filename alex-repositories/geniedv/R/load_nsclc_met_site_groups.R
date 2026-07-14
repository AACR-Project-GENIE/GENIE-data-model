#' Load the NSCLC metastasis site group classification
#'
#' Reads the package data file `inst/extdata/nsclc_met_site_groups.tsv` which
#' maps ICD-O-3 topography codes to distant metastasis site groups for NSCLC.
#'
#' @returns A data frame with columns `numeric_code`, `icdo3_site`,
#'   `classification`, and `mets_site_group`.
#' @export
#'
#' @examples
#' head(load_nsclc_met_site_groups())
load_nsclc_met_site_groups <- function() {
  path <- system.file(
    "extdata",
    "nsclc_met_site_groups.tsv",
    package = "geniedv"
  )
  if (!nzchar(path)) {
    cli::cli_abort(
      "Could not locate {.file extdata/nsclc_met_site_groups.tsv} in geniedv."
    )
  }

  readr::read_tsv(path, show_col_types = FALSE)
}
