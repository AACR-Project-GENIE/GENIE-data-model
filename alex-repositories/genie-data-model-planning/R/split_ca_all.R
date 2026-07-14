split_ca_all <- function(ca_all) {
  ca_ind <- ca_all %>%
    filter(redcap_ca_index %in% "Yes")
  ca_non_ind <- ca_all %>%
    filter(redcap_ca_index %in% "No")

  ca_ambiguous <- ca_all %>%
    filter(tr_eligible %in% 1) %>%
    filter(!(redcap_ca_index %in% c("No", "Yes")))

  if (nrow(ca_ambiguous) > 0) {
    cli_alert_danger(
      "There are some cases with redcap_ca_index left blank but tr_eligible = 1...QC issue."
    )
  }

  return(list(ca_ind = ca_ind, ca_non_ind = ca_non_ind))
}
