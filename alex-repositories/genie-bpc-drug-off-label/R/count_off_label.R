count_off_label <- function(
  dat_det,
  group_vars = c('cohort', 'agent')
) {
  dat_det %>%
    group_by(across(all_of(group_vars))) %>%
    summarize(
      n_uses = n(),
      n_on_label = sum(valid_ind_exists, na.rm = T),
      n_off_label = n_uses - n_on_label,
      prop_off_label = n_off_label / n_uses, 
      .groups = "drop"
    )
}
