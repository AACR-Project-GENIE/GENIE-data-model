drugs_ca_rearrangement <- function(
  dat,
  prefix = "^drugs_ca"
) {
  rel_cols <- colnames(dat)[
    stringr::str_detect(colnames(dat), prefix)
  ]

  if (length(rel_cols) %in% 0) {
    cli_abort("No drugs_ca columns in this dataset - fix!")
  }

  rtn <- dat %>%
    pivot_longer(
      cols = all_of(rel_cols),
      names_to = 'redcap_ca_seq',
      values_to = '.affected_cancer'
    ) %>%
    mutate(redcap_ca_seq = readr::parse_number(redcap_ca_seq)) %>%
    filter(.affected_cancer %in% 1) %>%
    drop_dots(.)

  rtn %<>%
    dplyr::relocate(
      redcap_ca_seq,
      .after = redcap_repeat_instance
    )

  return(rtn)
}
