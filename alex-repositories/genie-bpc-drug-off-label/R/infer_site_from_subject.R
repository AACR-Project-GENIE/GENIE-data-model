infer_site_from_subject <- function(vec_record_id) {
  vec_record_id %>%
    str_split(., pattern = "-") %>%
    purrr::map_chr(
      .x = .,
      .f = \(a) {
        a[2]
      }
    )
}
