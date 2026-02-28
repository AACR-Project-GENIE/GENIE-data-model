convert_dat_col_types <- function(
  dat,
  dict,
  print_cols = F
) {
  common_cols <- intersect(colnames(dat), dict$field_name)

  manifest <- dict %>%
    filter(field_name %in% common_cols) %>%
    mutate(
      letter_code = case_when(
        col_read_type %in% "char" ~ 'c',
        col_read_type %in% "numeric" ~ 'd',
        col_read_type %in% "logical" ~ 'l',
        col_read_type %in% 'dttm' ~ 'T',
        T ~ "ERROR"
      )
    )

  if (any(manifest$letter_code %in% "ERROR")) {
    first_error <- manifest %>%
      filter(letter_code %in% "ERROR") %>%
      slice(1) %>%
      pull(col_read_type)
    cli_abort(
      'Unrecognized col_read_type "{first_error}"- edit function to add.'
    )
  }

  if (print_cols) {
    cli_inform(
      "Converting {length(common_cols)} columns: {paste(common_cols, collapse = ', ')}"
    )
  }

  col_spec = as.list(manifest$letter_code)
  names(col_spec) <- manifest$field_name
  col_spec <- do.call(cols, args = col_spec)

  rtn <- type_convert(
    dat,
    col_types = col_spec
  )

  rtn
}
