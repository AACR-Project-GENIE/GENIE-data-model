pull_composite_id <- function(tables, table_name, ...) {
  tables %>%
    .[[table_name]] %>%
    tidyr::unite(".id", ..., sep = "|", remove = FALSE) |>
    dplyr::pull(.id)
}
