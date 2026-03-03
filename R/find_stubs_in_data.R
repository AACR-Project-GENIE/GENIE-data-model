find_stubs_in_data <- function(dat_cols) {
  stub_vars <- tibble(
    stub = dat_cols %>%
      .[str_detect(., '\\_\\_')]
  ) %>%
    separate(stub, into = c('stub', 'num'), sep = '\\_\\_\\_') %>%
    mutate(num = as.numeric(num)) %>%
    group_by(stub) %>%
    summarize(
      min = min(num),
      max = max(num)
    )

  return(stub_vars)
}
