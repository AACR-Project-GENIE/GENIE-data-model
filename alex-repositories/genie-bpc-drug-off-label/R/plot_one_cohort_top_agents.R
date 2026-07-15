plot_one_cohort_top_agents <- function(
  dat,
  cohort_lab_var,
  agent_lab_var,
  x_scale_trans_fn = 'identity',
  x_scale_name = "Exposures",
  x_breaks = waiver(),
  on_lab = "On-label",
  off_lab = "Off-label"
) {
  dat_long <- dat %>%
    mutate(n_on = -n_on) %>%
    select(
      all_of(
        c(cohort_lab_var, agent_lab_var)
      ),
      agent_order,
      n_off,
      n_on
    ) %>%
    pivot_longer(
      cols = c(n_off, n_on),
      names_to = 'cat',
      values_to = 'n'
    )

  dat_long %<>%
    arrange(desc(cat)) %>%
    mutate(
      cat = case_when(
        cat %in% 'n_off' ~ off_lab,
        cat %in% 'n_on' ~ on_lab
      ),
      cat = fct_inorder(cat)
    )

  dat_long %<>%
    arrange(agent_order) %>%
    mutate('{agent_lab_var}' := fct_rev(fct_inorder(.data[[agent_lab_var]])))

  gg <- ggplot(
    dat_long,
    aes(
      fill = cat,
      groups = .data[[agent_lab_var]],
      y = .data[[agent_lab_var]],
      x = n
    )
  ) +
    geom_bar(position = 'stack', stat = 'identity', orientation = 'y') +
    scale_x_continuous(
      transform = x_scale_trans_fn,
      breaks = x_breaks,
      name = x_scale_name
    ) +
    scale_y_discrete(name = NULL) +
    scale_fill_d3(name = NULL) +
    theme_bw() +
    facet_wrap(vars(.data[[cohort_lab_var]])) +
    theme(
      legend.position = 'bottom'
    )

  gg
}
