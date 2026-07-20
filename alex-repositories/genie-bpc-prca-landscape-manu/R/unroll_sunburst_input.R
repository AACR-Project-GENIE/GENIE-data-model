
unroll_sunburst_input <- function(dat) {
  rtn <- dat %>%
    mutate(
      unrolled = purrr::map2(
        .x = path,
        .y = Pop,
        .f = unroll_one_row
      )
    ) %>%
    select(unrolled) %>%
    unnest(unrolled)
  
  return(rtn)
  
}

unroll_one_row <- function(path, pop) {
  if (length(path) != 1 & length(pop) != 1) {
    cli_abort('Only works on one row')
  }
  
  tibble(
    drug = str_split(path, "-")[[1]]
  ) %>%
    mutate(
      seq = 1:n(),
      n = pop
    )
}
