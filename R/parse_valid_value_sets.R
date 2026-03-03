# Takes the valid value format in prissmm and turns it into named (magic number keys) character vectors (text values).
parse_valid_value_sets <- function(
  str,
  add_na = T
) {
  list_of_str <- stringr::str_split(str, pattern = "\\|")
  purrr::map(
    .x = list_of_str,
    .f = \(x) {
      key <- purrr::map_chr(
        x,
        ~ stringr::str_trim(
          stringr::str_split_i(.x, pattern = "\\, ", i = 1)
        )
      )
      val <- purrr::map_chr(
        x,
        ~ stringr::str_trim(
          stringr::str_split_i(.x, pattern = "\\, ", i = 2)
        )
      )
      if (add_na) {
        key <- c(key, NA)
        val <- c(val, NA)
      }
      names(val) <- key
      val
    }
  )
}
