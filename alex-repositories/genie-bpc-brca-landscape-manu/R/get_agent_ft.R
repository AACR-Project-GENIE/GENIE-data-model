get_agent_ft <- function(dat) {
  dat %<>% tabyl(agent) %>%
    adorn_totals(.) %>%
    mutate(
      percent = round(percent*100),
      str = glue("{n} ({percent}%)")
    ) %>%
    mutate(is_total = agent %in% "Total") %>%
    arrange(is_total, desc(n)) %>%
    select(agent, `n (%)` = str) 
  
  nr <- nrow(dat)
  
  dat %>%
    flextable(.) %>%
    theme_booktabs(.) %>%
    align(j = 2, part = "all", align = "right") %>%
    bold(i = nr, part = "body", bold = T) %>%
    autofit(.)
}
