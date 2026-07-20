# Takes the yaml file supplied and limits the table to that.
apply_column_scope <- function(tbl_list, scope_dir) {
  yaml_files <- fs::dir_ls(scope_dir, glob = "*.yaml")

  scope <- purrr::map(yaml_files, \(f) {
    cfg <- yaml::read_yaml(f)
    list(
      columns = names(purrr::keep(cfg$columns, isTRUE)),
      sort_by = cfg$sort_by
    )
  }) |>
    purrr::set_names(tools::file_path_sans_ext(basename(yaml_files)))

  purrr::imap(tbl_list, \(tbl, nm) {
    cfg <- scope[[nm]]
    if (is.null(cfg)) {
      cli::cli_warn(
        "No scope file found for table {.val {nm}} — keeping all columns."
      )
      return(tbl)
    }

    keep_cols <- cfg$columns
    missing_cols <- setdiff(keep_cols, names(tbl))
    if (length(missing_cols) > 0) {
      cli::cli_warn(
        "Table {.val {nm}}: scoped columns not present and skipped: {.val {missing_cols}}"
      )
    }

    sort_cols <- cfg$sort_by
    missing_sort <- setdiff(sort_cols, names(tbl))
    if (length(missing_sort) > 0) {
      cli::cli_abort(
        "Table {.val {nm}}: sort columns not found: {.val {missing_sort}}"
      )
    }

    tbl |>
      dplyr::select(dplyr::any_of(keep_cols)) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(sort_cols)))
  })
}
