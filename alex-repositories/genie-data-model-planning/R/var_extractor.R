# Takes a tibble and extracts some information about all the columns.
var_extractor <- function(
  dat,
  missing_levels = ""
) {
  extra_wide <- dat %>%
    summarize(
      across(
        .cols = everything(),
        .fns = list(
          SSEEPP_present = \(x) TRUE,
          SSEEPP_n_vals = \(x) length(unique(x)),
          SSEEPP_n_missing = \(x) sum(is.na(x) | x %in% missing_levels)
        )
      )
    )

  extra_wide %>%
    pivot_longer(cols = everything()) %>%
    separate(col = name, into = c('var', 'colname'), sep = "_SSEEPP_") %>%
    pivot_wider(
      names_from = colname,
      values_from = value
    ) %>%
    mutate(present = as.logical(present))
}
