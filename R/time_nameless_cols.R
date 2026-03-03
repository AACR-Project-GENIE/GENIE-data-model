trim_nameless_cols <- function(dat) {
  dat %>%
    select(-matches("^\\.\\.\\."))
}
