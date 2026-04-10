#' Rearrange drugs-by-cancer indicator columns into long form
#'
#' The raw regimen form has one row per regimen with a set of wide `drugs_ca*`
#' indicator columns, one per cancer diagnosis, marking which cancer(s) a
#' regimen was directed at. This helper pivots those indicators into long form
#' so that each row represents one (regimen, cancer) association, keyed by a new
#' `redcap_ca_seq` column parsed from the original column names. Rows where the
#' indicator is not `1` (i.e. regimens not directed at that cancer) are dropped.
#'
#' @param dat A regimen-level data frame containing the wide `drugs_ca*`
#'   indicator columns and a `redcap_repeat_instance` column.
#' @param prefix A regex used to identify the `drugs_ca*` indicator columns to
#'   pivot. Defaults to `"^drugs_ca"`.
#'
#' @returns `dat` pivoted to long form with one row per regimen-cancer
#'   association, a new integer `redcap_ca_seq` column placed after
#'   `redcap_repeat_instance`, and the original wide indicator columns removed.
#' @export
#'
#' @examples
#' # drugs_ca_rearrangement(reg)
drugs_ca_rearrangement <- function(
  dat,
  prefix = "^drugs_ca"
) {
  rel_cols <- colnames(dat)[
    stringr::str_detect(colnames(dat), prefix)
  ]

  if (length(rel_cols) %in% 0) {
    cli_abort("No drugs_ca columns in this dataset - fix!")
  }

  rtn <- dat %>%
    pivot_longer(
      cols = all_of(rel_cols),
      names_to = 'redcap_ca_seq',
      values_to = '.affected_cancer'
    ) %>%
    mutate(redcap_ca_seq = readr::parse_number(redcap_ca_seq)) %>%
    filter(.affected_cancer %in% 1) %>%
    drop_dots(.)

  rtn %<>%
    dplyr::relocate(
      redcap_ca_seq,
      .after = redcap_repeat_instance
    )

  return(rtn)
}
