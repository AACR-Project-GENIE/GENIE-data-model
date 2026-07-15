
reshape_drug_compliments <- function(df_dc, k = 3) {
  df_dc %<>% arrange(desc(n))
  
  ordinal_words_vec <- c("first", "second", "third", "fourth", "fifth",
                         "sixth", "seventh", "eighth", "ninth", "tenth")
  
  df_dc %>%
    slice(1:k) %>%
    mutate(
      order = ordinal_words_vec[1:min(k, n())],
      # remove synonyms:
      drug = str_replace(drug, "\\(.*\\)", ""),
      str = glue("{drug} ({formatC(prop*100,1, format = 'f', digits = 1)}%)")
    ) %>%
    select(order, str) %>%
    pivot_wider(names_from = "order", values_from = "str")
}