aggregate_ddr_by_cancer_type <- function(
  dat
) {
  # cts = cancer type summary
  cts <- dat %>%
    group_by(custom_cancer_type) %>%
    rename(ddr = any_ddr) %>%
    summarize(
      across(
        .cols = c(any_of(custom_ddr_list()), ddr),
        .fns = list(
          # proportion of samples positive out of those tested:
          pos = \(x) {
            sum(x, na.rm = T) / sum(!is.na(x))
          },
          # proportion of samples tested:
          test = \(x) {
            sum(!is.na(x)) / n()
          }
        )
      )
    )

  # This block turns the ddr features into text and then gets rid of them:
  cts %<>%
    mutate(
      custom_cancer_type_pos_lab = glue(
        "{custom_cancer_type}, {formatC(ddr_pos*100, digits = 0, format = 'f')}%)"
      ),
      custom_cancer_type_test_lab = glue(
        "{custom_cancer_type}, {formatC(ddr_test*100, digits = 0, format = 'f')}%)"
      ),
      custom_cancer_type_pos_lab = fct_inorder(custom_cancer_type_pos_lab),
      custom_cancer_type_test_lab = fct_inorder(custom_cancer_type_test_lab)
    )

  cts_long <- cts %>%
    pivot_longer(
      cols = -contains("custom_cancer_type"),
      names_to = "feature",
    )

  cts_long %<>%
    separate_wider_delim(
      feature,
      delim = "_",
      names = c('feature', 'metric'),
      too_many = "error",
      too_few = 'error'
    ) %>%
    pivot_wider(
      names_from = 'metric',
      values_from = 'value'
    )

  cts_long %<>%
    mutate(
      feature = if_else(feature %in% 'ddr', 'any_DDR', feature)
    ) %>%
    arrange(
      desc(feature %in% 'any_DDR'),
      feature
    ) %>%
    mutate(feature = fct_inorder(feature))

  cts_long
}
