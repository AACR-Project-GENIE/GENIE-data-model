check_test_cols_true_false <- function(
    dat
) {
  test_result <- dat %>%
    select(matches("^test_")) %>%
    summarize(
      across(
        .cols = everything(),
        .fns = \(x) all(x %in% c(T,F))
      )
    ) %>%
    pivot_longer(
      cols = everything()
    ) %>%
    pull(value) %>%
    all(.)
  
  if (!test_result) {
    cli_abort("^test_ columns contain values other than T/F.")
  }
}
