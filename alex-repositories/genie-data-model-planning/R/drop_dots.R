drop_dots <- function(x) {
  x %>% select(-tidyselect::starts_with("."))
}

# test <- tibble(
#   a = 1, b = 2, .c = 3, asd908 = 4, .asdf = 5
# )
# test
# test %>% drop_dots
