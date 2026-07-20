remap_ca_seq <- function(tables, ref_tables) {
  # Build cancer_hash -> new ca_seq from ref_tables

  ref_hash_map <- dplyr::bind_rows(
    dplyr::select(ref_tables$ca_ind, cancer_hash, ca_seq),
    dplyr::select(ref_tables$ca_non_ind, cancer_hash, ca_seq)
  )

  if (anyDuplicated(ref_hash_map$cancer_hash)) {
    cli::cli_abort("Duplicate {.field cancer_hash} values found in {.arg ref_tables}.")
  }

  # Build cancer_hash -> old ca_seq from tables (before remapping)
  old_hash_map <- dplyr::bind_rows(
    dplyr::select(tables$ca_ind, cancer_hash, record_id, ca_seq),
    dplyr::select(tables$ca_non_ind, cancer_hash, record_id, ca_seq)
  )

  if (anyDuplicated(old_hash_map$cancer_hash)) {
    cli::cli_abort("Duplicate {.field cancer_hash} values found in {.arg tables}.")
  }

  # Build record_id + old ca_seq -> new ca_seq for non-cancer tables
  seq_map <- old_hash_map |>
    dplyr::left_join(
      dplyr::select(ref_hash_map, cancer_hash, new_ca_seq = ca_seq),
      by = "cancer_hash"
    ) |>
    dplyr::select(record_id, ca_seq, new_ca_seq)

  purrr::map(tables, \(tbl) {
    has_hash <- "cancer_hash" %in% colnames(tbl)
    has_ca_seq <- "ca_seq" %in% colnames(tbl)

    if (has_hash) {
      # Cancer tables: join directly on cancer_hash
      tbl |>
        dplyr::select(-ca_seq) |>
        dplyr::left_join(ref_hash_map, by = "cancer_hash")
    } else if (has_ca_seq && "record_id" %in% colnames(tbl)) {
      # Non-cancer tables: remap via record_id + old ca_seq
      tbl |>
        dplyr::left_join(seq_map, by = c("record_id", "ca_seq")) |>
        dplyr::mutate(ca_seq = new_ca_seq) |>
        dplyr::select(-new_ca_seq)
    } else {
      tbl
    }
  })
}
