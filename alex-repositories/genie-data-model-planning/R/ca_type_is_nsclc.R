ca_type_is_nsclc <- function(x) {
  x %>%
    mutate(
      chk = ca_type %in% '51'
    )
}
