# A sloppy function structurally that I keep finding a need for:
# Make a very rigid 2x2 flextable object.
# The reason I want this is making a grid of flextable-based grobs.
ft_two_by_two <- function(
    dat,
    var1,
    var2,
    add_odds_ratio = T,
    or_decimals = 1
) {
  nums <- tabyl(dat, !!sym(var1), !!sym(var2)) %>%
    adorn_totals(where = "both") %>%
    as_tibble(.) %>%
    # rearrange so the "true" or 1 (positive) values are top left (convention)
    select(1,3,2,Total) %>%
    slice(c(2,1,3))
  
  if (add_odds_ratio) {
    or_test <- fisher_test_helper(
      cell_11 = nums[[1, 2]],
      cell_10 = nums[[1, 3]],
      cell_01 = nums[[2, 2]],
      cell_00 = nums[[2, 3]],
      alpha = 0.05
    )
    
    or_str <- glue("OR = {formatC(or_test$estimate, format = 'f', digits = 2)} ({formatC(or_test$conf.low, format = 'f', digits = 2)}, {formatC(or_test$conf.high, format = 'f', digits = 2)})")
    
  }
  
  ft <- nums %>%
    rename(` ` = 1) %>%
    mutate(col1 = c(rep(var1, 2), "")) %>%
    relocate(col1, .before = 0L) %>%
    rename(`  ` = col1)
  
  ft <- ft %>%
    flextable(.) %>%
    add_header_row(top = T, values = c("", "", rep(var2, 2), "")) %>%
    merge_v(j = 1) %>%
    merge_at(part = 'header', i = 1, j = 3:4) %>%
    border_remove() %>%
    autofit(.) %>%
    align(j = 1:2, align = 'right') %>%
    align(j = 5, align = 'left') %>%
    align(i = 1, part = 'header', align = 'center') %>%
    align(i = 2, part = 'header', align = 'left')
  
  if (add_odds_ratio) {
    ft <- ft |> 
      add_body_row(top = F, values = c("", rep(or_str, 4))) |>
      merge_at(i = 4, j = 2:5) |>
      # Had to take this away because it was causing plotting bugs:
      # italic(i = 4) |>
      align(i = 4, align = "center")
  }
  
  ft <- ft %>%
    hline(i = 1:2, j = 3:4, border = fp_border_default()) %>%
    hline_top(j = 3:4, border = fp_border_default()) %>%
    # hline(i = 0) %>%
    hline(part = "header", i = 1, j = 3:4) %>%
    vline(i = 1:2, j = 1:4, part = "body") %>%
    align(i = 1:3, j = 3:4, align = "left")
  
  
  return(ft)
  
}