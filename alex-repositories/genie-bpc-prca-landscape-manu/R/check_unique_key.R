

#' ... The key variables.  For example, ca_seq and record_id.
check_unique_key <- function(
    dat,
    ...
) {
  top_count <- dat %>%
    count(..., sort = T) %>%
    slice(1)
  
  if (top_count$n > 1) {
    print(top_count)
    cli::cli_alert_danger("Duplicate keys ^.  Top hit printed above")
    return(F)
  } else {
    return(T)
  }
}