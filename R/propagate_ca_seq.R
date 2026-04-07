#' Propagate the cancer sequence derivation from the cancer dx tables to the others
#'
#' @param lst A list of dataframes representing PRISSMM tables. The cancer index table must be called `ca_ind` and the non-index cancer table must be called `ca_non_ind`. Other than that it's based on column presence/absence.
#'
#' @returns `lst` with the `ca_seq` column added to relevant dataframes.
#' @export
#'
#' @examples
propagate_ca_seq <- function(lst) {
  ca_seq_lookup <- bind_rows(
    lst[["ca_ind"]],
    lst[["ca_non_ind"]]
  ) |>
    select(record_id, redcap_ca_seq, ca_seq)

  ca_tables <- c("ca_ind", "ca_non_ind")
  purrr::imap(lst, \(tab, nm) {
    if (nm %in% ca_tables) {
      return(tab)
    }
    if (!("redcap_ca_seq" %in% names(tab))) {
      return(tab)
    }
    if ('ca_seq' %in% names(tab)) {
      cli::cli_alert(
        "Skipping some tables that have redcap_ca_seq and ca_seq already added (means this function was probably already applied)"
      )
      return(tab)
    }
    tab |>
      dplyr::left_join(ca_seq_lookup, by = c("record_id", "redcap_ca_seq")) |>
      relocate(ca_seq, .after = record_id)
  })
}
