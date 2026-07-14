long_dat_to_mat_helper <- function(
  dat,
  id_col = 'sample_id',
  val_col = 'altered'
) {
  dat_wide <- dat %>%
    select(all_of(c(id_col, val_col, 'hugo_symbol'))) %>%
    pivot_wider(
      names_from = 'hugo_symbol',
      values_from = val_col
    ) %>%
    select(all_of(id_col), sort(tidyselect::peek_vars()))

  dat_mat <- dat_wide %>%
    select(-all_of(id_col)) %>%
    as.matrix(.)

  rownames(dat_mat) <- dat_wide[[id_col]]

  dat_mat
}
