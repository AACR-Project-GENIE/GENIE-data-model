# Add the agent order
agent_plot_label_helper <- function(
  dat,
  group_vars,
  n_agents_per_group,
  other_lab = "All other exposures",
  lab_name = "agent_lab"
) {
  rtn <- dat %>%
    group_by({{ group_vars }}) %>%
    arrange(desc(n_off)) %>%
    mutate(
      .agent_order = case_when(
        row_number() <= n_agents_per_group ~ 1:n(),
        T ~ n_agents_per_group + 1
      )
    )

  rtn %<>%
    group_by(.agent_order, .add = TRUE) %>%
    mutate(
      .agent_proto_lab = case_when(
        n() > 1 ~ other_lab,
        T ~ agent
      )
    ) %>%
    ungroup(.) %>%
    group_by({{ group_vars }}, .agent_proto_lab) %>%
    summarize(
      .new_lab = glue(
        '{first(.agent_proto_lab)}: {sum(n_off)}/{sum(n_exposures)} ({round(sum(n_off)/sum(n_exposures) * 100, 0)}%)'
      ),
      agent_order = first(.agent_order),
      across(
        .cols = c(n_on, n_off, n_exposures),
        .fns = sum
      ),
      .groups = 'drop'
    )

  rtn %>% mutate('{lab_name}' := .new_lab) %>% drop_dots
}
