library(reprex)

library(pointblank)
al_with_message <-
  action_levels(
    warn_at = 0.1,
    fns = list(
      warn = ~ log4r_step(
        x,
        message = "Step {x$i} had {x$n_passed} passing units."
      )
    )
  )
al_with_message
agent <-
  create_agent(
    tbl = small_table,
    tbl_name = "small_table",
    label = "VALID-I Example No. 2",
    actions = al_with_message
  ) %>%
  col_vals_lt(a, value = 7) %>%
  interrogate()

get_agent_x_list(agent)$n_passed
get_agent_x_list(agent)$stop
