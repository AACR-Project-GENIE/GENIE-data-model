
helper_max_int <- function(
    dat,
    new_var_name,
    new_value_name
) {
  
  rtn <- dat %>%
    group_by(record_id, ca_seq) %>%
    arrange(desc(value_yrs)) %>%
    slice(1) %>%
    ungroup
  
  rtn %<>%
    rename(
      {{new_var_name}} := var,
      {{new_value_name}} := value_yrs
    )
  
  return(rtn)
  
}