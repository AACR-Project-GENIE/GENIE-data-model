# A series of functions that take the hdrug data and turn it into a nicer
#   table summary.
get_cohort_drug_summary <- function(
    dat_hdrug_det,
    include_total = T
) {
  rtn <- dat_hdrug_det %>% 
    group_by(cohort, agent) %>%
    summarize(
      off_label = sum(!valid_ind_exists, na.rm = T),
      exposures = n(),
      prop = off_label/exposures,
      str = n_pct_str(off_label, exposures, digits = 0, show_d = T),
      .groups = 'drop'
    )
  
  if (include_total) {
    tot <- rtn %>%
      group_by(cohort) %>%
      summarize(
        off_label = sum(off_label),
        exposures = sum(exposures),
        .groups = 'drop'
      ) %>%
      mutate(
        agent = 'total',
        prop = off_label/exposures,
        str = n_pct_str(off_label, exposures, digits = 0, show_d = T)
      )
    
    rtn <- bind_rows(rtn, tot) %>%
      arrange(cohort, desc(agent %in% 'total'), agent)
  }
  
  return(rtn)
  
}

limit_drug_sum <- function(
    dat_drug_sum,
    drugs_per_cohort = 3
) {
  
  # hacky way to allow the total row to not count:
  has_total_row <- any(tolower(dat_drug_sum$agent) %in% "total")
  slice_rows <- if_else(
    has_total_row,
    drugs_per_cohort + 1, 
    drugs_per_cohort
  )
  
  rtn <- dat_drug_sum %>%
    group_by(cohort) %>%
    arrange(desc(off_label)) %>%
    slice(1:slice_rows) %>%
    ungroup(.)
  
  return(rtn)
}

drug_name_display_trim <- function(
    agent_vec
) {
  agent_vec <- str_replace_all(
    agent_vec, "Ditosylate|Acetate|Disodium|Dimaleate", ""
  )
  agent_vec <- str_replace_all(agent_vec, "liposome", 'lip')
  agent_vec <-  str_trim(agent_vec)
  
  return(agent_vec)
  
}


drug_cohort_flextable <- function(
    dc_tidy,
    style = 'zebra'
) {
  disp_tab <- dc_tidy %>%
    group_by(cohort) %>%
    mutate(
      col = case_when(
        agent %in% 'total' ~ "Total",
        T ~ paste0("Drug ", row_number() - 1)
      ),
      drug_str = if_else(
        agent %in% "total",
        str,
        paste0(agent, ": ", str)
      )
    ) %>%
    ungroup(.) %>%
    mutate(col = fct_inorder(col))
  
  disp_tab %<>%
    select(cohort, col, drug_str) %>%
    pivot_wider(
      names_from = col,
      values_from = drug_str
    )
  
  sum_row <- dc_tidy %>%
    filter(agent %in% "total") %>%
    summarize(
      off_label = sum(off_label),
      exposures  = sum(exposures),
      str = n_pct_str(off_label, exposures, digits = 0, show_d = T)
    )
  
  disp_tab %<>%
    add_row(
      cohort = "All",
      Total = sum_row$str
    )
    
      
  
  disp_tab <- disp_tab %>%
    flextable(.) %>%
    theme_zebra(.) %>%
    border(j = 2, 
           border.right = officer::fp_border(color = "gray40"), 
           part = 'all') %>%
    bold(j = 2, part = 'body')
  
  
  if (style %in% 'zebra') {
    disp_tab <- disp_tab %>%
      autofit(.)
  } else if (style %in% 'bleh') {
  } else {
    cli_abort("'style' argument is invalid.")
  }
  
  return(disp_tab)
}