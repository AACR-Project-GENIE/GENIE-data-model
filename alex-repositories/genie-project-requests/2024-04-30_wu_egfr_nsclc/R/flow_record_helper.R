flow_record_helper <- function(dat, message, l_dat = NULL) {
  new_row <- tibble(
    message = message,
    dat = list(dat %>% select(record_id) %>% distinct)
  )
  
  if (!is.null(l_dat)) {
    l_dat <- bind_rows(l_dat, new_row)
  } else {
    l_dat <- new_row
  }
  
  return(l_dat)
  
}
