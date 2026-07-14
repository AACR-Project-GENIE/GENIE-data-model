restrict_to_shared_unique <- function(tables_1, tables_2, spec) {
  for (tbl_name in names(spec)) {
    key_cols <- spec[[tbl_name]]
    t1 <- tables_1[[tbl_name]]
    t2 <- tables_2[[tbl_name]]

    unique_keys_1 <- t1 |>
      dplyr::count(dplyr::across(dplyr::all_of(key_cols))) |>
      dplyr::filter(n == 1) |>
      dplyr::select(-n)

    unique_keys_2 <- t2 |>
      dplyr::count(dplyr::across(dplyr::all_of(key_cols))) |>
      dplyr::filter(n == 1) |>
      dplyr::select(-n)

    shared_keys <- dplyr::inner_join(
      unique_keys_1,
      unique_keys_2,
      by = key_cols
    )

    tables_1[[tbl_name]] <- dplyr::semi_join(t1, shared_keys, by = key_cols)
    tables_2[[tbl_name]] <- dplyr::semi_join(t2, shared_keys, by = key_cols)
  }
  list(tables_1 = tables_1, tables_2 = tables_2)
}
