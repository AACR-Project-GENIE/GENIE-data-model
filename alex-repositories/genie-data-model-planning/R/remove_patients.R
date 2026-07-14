remove_patients <- function(tables, record_ids) {
  purrr::map(tables, \(tbl) {
    if ("record_id" %in% colnames(tbl)) {
      dplyr::filter(tbl, !(record_id %in% record_ids))
    } else {
      tbl
    }
  })
}
