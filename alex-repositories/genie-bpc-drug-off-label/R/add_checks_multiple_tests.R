add_check_multiple_tests <- function(
    dat_poss_app,
    test_cols_to_include = NULL
) {
  
  # Add an "all" test.
  if (is.null(test_cols_to_include)) {
    test_cols_to_include <- names(dat_poss_app)[
      str_detect(names(dat_poss_app), "^test_")
    ]
  }
  
  dat_poss_app %<>%
    mutate(
      test_all = as.logical(pmin(!!!rlang::syms(test_cols_to_include)))
    )
  
  
  return(dat_poss_app)
}
