pull_all_results <- function(result_dir) {
  rtn <- tibble(
    file = dir(result_dir),
  ) %>%
    mutate(
      path = path(result_dir, file),
      dat_name = str_sub(file, 4, -5),
      dat_qc_res = purrr::map(.x = path, .f = readr::read_rds)
    )

  rtn <- rtn %>%
    select(dat_qc_res) %>%
    unnest(dat_qc_res)

  rtn
}


display_results_summary <- function(result_dir, type = 'return') {
  rtn <- pull_all_results(result_dir)

  rtn %<>% select(qc_layer, site, dat_name, all_passed)

  if (type %in% 'return') {
    return(rtn)
  } else {
    rtn %>%
      print(.)

    invisible(result_dir)
  }
}
