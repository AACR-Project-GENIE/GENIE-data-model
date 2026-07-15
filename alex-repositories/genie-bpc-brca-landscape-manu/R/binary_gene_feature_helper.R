binary_gene_feature_helper <- function(vec, zero = "Wild Type", one = "Altered") {
  case_when(
    vec %in% 0 ~ zero,
    vec %in% 1 ~ one,
    T ~ "error"
  ) %>%
    factor(., levels = c(zero, one))
}