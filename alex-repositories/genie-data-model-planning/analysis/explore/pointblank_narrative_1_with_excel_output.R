library(pointblank)

# VALID-I
small_table_custom <- small_table
small_table_custom[1, 'a'] <- 11

agent <-
  create_agent(
    tbl = small_table_custom,
    tbl_name = "small_table (with an a fail added)",
    label = "VALID-I Example No. 1"
  ) %>%
  col_is_posix(date_time) %>%
  col_vals_in_set(f, set = c("low", "mid", "high")) %>%
  col_vals_lt(a, value = 10) %>%
  col_vals_regex(b, regex = "^[0-9]-[a-z]{3}-[0-9]{3}$") %>%
  col_vals_between(d, left = 0, right = 5000) %>%
  interrogate()

agent

# To get the available CSV for the fail rows do:
get_data_extracts(agent, i = 5)
# Provides a list of all rows with failures on certain tests.
get_data_extracts(agent, i = NULL)
# What this is missing is data on what test was failed.  I think you could just merge this in by combining this with...
agent$validation_set

# This also seems useful:
get_sundered_data(agent, type = 'fail')
get_sundered_data(agent, type = 'pass')

# Using action levels (I subbed in the logging version):
al <-
  action_levels(
    warn_at = 0.1, # scalars are interpretted as a percentage.
    stop_at = 0.2,
    fns = list(
      warn = ~ log4r_step(
        x,
        append_to = here('analysis', 'explore', 'test_log')
      ),
      stop = ~ log4r_step(
        x,
        append_to = here('analysis', 'explore', 'test_log')
      )
    )
  )
al # prints nicely in the console if you find that useful.d
agent <-
  create_agent(
    tbl = small_table,
    tbl_name = "small_table",
    label = "VALID-I Example No. 2",
    actions = al
  ) %>%
  col_is_posix(date_time) %>%
  col_vals_in_set(f, set = c("low", "mid")) %>%
  col_vals_lt(a, value = 7) %>%
  col_vals_regex(b, regex = "^[0-9]-[a-w]{3}-[2-9]{3}$") %>%
  col_vals_between(d, left = 0, right = 4000) %>%
  interrogate()

agent # way better printing

# You can create a function with a custom side effect instead fo log4r_step.
# I'll probably want to write one for excel.
# This is the stuff that you can take advantage of with those functions:
x <- get_agent_x_list(agent, i = 2)
lue::glue(
  "In Step {x$i}, there were {x$n} test units and {x$f_failed * 100}% \\
  failed. STOP condition met: {tolower(x$stop)}."
)
rm(x)

# Now to try putting this into the log:
al <-
  action_levels(
    # these are defaults:
    warn_at = 0.1, # scalars are interpretted as a percentage.
    stop_at = 0.2,
    fns = list(
      warn = ~ log4r_step(
        x,
        message = glue::glue(
          "In Step {x$i}, there were {x$n} test units and {round(x$f_failed * 100, 0)}% \\
  failed. STOP condition met: {tolower(x$stop)}."
        ),
        append_to = here('analysis', 'explore', 'test_log')
      ),
      stop = ~ log4r_step(
        x,
        message = glue::glue(
          "In Step {x$i}, there were {x$n} test units and {round(x$f_failed * 100, 0)}% \\
  failed. STOP condition met: {tolower(x$stop)}."
        ),
        append_to = here('analysis', 'explore', 'test_log')
      )
    )
  )
al
agent <-
  create_agent(
    tbl = small_table,
    tbl_name = "small_table",
    label = "VALID-I Example No. 2",
    actions = al
  ) %>%
  col_is_posix(date_time) %>%
  col_vals_in_set(f, set = c("low", "mid")) %>%
  col_vals_lt(a, value = 7) %>%
  col_vals_regex(b, regex = "^[0-9]-[a-w]{3}-[2-9]{3}$") %>%
  col_vals_between(d, left = 0, right = 4000) %>%
  interrogate()

# I think this is a bug.  The code doesn't seem to use the message arguemnt at at all:
# https://github.com/rstudio/pointblank/blob/0151630fdf94ff0ff012e688016d8330eacd9f70/R/logging.R#L242
# That's a pretty easy thing to try and fix.  Open source contribution?

agent # way better print

# I don't see a way to get get_data_extracts() pieces from the get_agent_x_list() function unfortunately, so we may have to combine later on.
# Here's one way to do that:
report_tab <- full_join(
  select(
    agent$validation_set,
    i,
    sha1, # not exactly sure but I think this maybe a unique ID.
    assertion_type,
    columns_expr,
    brief,
    time_processed
  ),
  mutate(
    bind_rows(
      get_data_extracts(agent),
      .id = 'i'
    ),
    i = as.integer(i)
  ),
  by = 'i'
)

report_tab

# Slightly fanicer, you could select only some columns from get_data_extracts().  Selecting the column with the offending value would be nice for this example but probably doesn't generalize.  Selecting primary keys could be better.
