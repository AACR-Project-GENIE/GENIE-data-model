
get_convert_vars <- function(
    dat,
    var_dat,
    days_per_year = 365.25,
    months_per_year = 12.0148 
) {
  rel_vars <- var_dat$var
  
  rtn <- dat %>% 
    select(
      record_id, ca_seq,
      any_of(rel_vars)
    ) 
  
  if (ncol(rtn) %in% 2) {
    return(
      tibble(
      record_id = character(0),
      ca_seq = double(0),
      var = character(0),
      value_yrs = double(0)
      )
    )
  } else {
    
    rtn %<>%
      pivot_longer(
        cols = -c(record_id, ca_seq),
        names_to = "var"
      )
    
    rtn %<>%
      left_join(
        .,
        select(var_dat, var, unit),
        by = 'var'
      ) %>%
      mutate(
        value_yrs = case_when(
          unit %in% "year" ~ value,
          unit %in% "month" ~ value / months_per_year,
          unit %in% "day" ~ value / days_per_year
        )
      )
    
    rtn %<>%
      select(record_id, ca_seq, var, value_yrs) %>%
      filter(!is.na(var) & !is.na(value_yrs))
    
    return(rtn)
    
  }
}
