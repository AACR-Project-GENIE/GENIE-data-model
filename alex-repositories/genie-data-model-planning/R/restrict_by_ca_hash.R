restrict_by_ca_hash <- function(tables, ca_hashes) {
  tables$ca_ind <- dplyr::filter(tables$ca_ind, cancer_hash %in% ca_hashes)
  tables$ca_non_ind <- dplyr::filter(tables$ca_non_ind, cancer_hash %in% ca_hashes)

  keep_ids <- union(tables$ca_ind$record_id, tables$ca_non_ind$record_id)

  purrr::map(tables, \(tbl) {
    if ("record_id" %in% colnames(tbl)) {
      dplyr::filter(tbl, record_id %in% keep_ids)
    } else {
      tbl
    }
  })
}
