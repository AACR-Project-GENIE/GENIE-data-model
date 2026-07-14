# Yes, AI.
# A function for to restrict to patients which are in both the new and old derived variable codes.  A convenience because I don't care about whatever reasons people were excluded, I care if my functions work.
restrict_to_shared_patients <- function(tables_legacy, tables_new) {
  ids_legacy <- purrr::map(tables_legacy, \(tbl) tbl$record_id) |>
    purrr::reduce(union)
  ids_new <- purrr::map(tables_new, \(tbl) tbl$record_id) |>
    purrr::reduce(union)

  shared_ids <- intersect(ids_legacy, ids_new)

  cli::cli_inform(c(
    "Restricting to patients present in both legacy and new tables:",
    "i" = "{length(setdiff(ids_legacy, shared_ids))} patient(s) removed from legacy tables.",
    "i" = "{length(setdiff(ids_new, shared_ids))} patient(s) removed from new tables.",
    "v" = "{length(shared_ids)} patient(s) retained."
  ))

  list(
    tables_legacy = purrr::map(tables_legacy, \(tbl) {
      dplyr::filter(tbl, record_id %in% shared_ids)
    }),
    tables_new = purrr::map(tables_new, \(tbl) {
      dplyr::filter(tbl, record_id %in% shared_ids)
    })
  )
}
