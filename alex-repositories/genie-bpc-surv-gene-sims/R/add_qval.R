add_qval <- function(coef_dat, adjust_for_missing = F) {
  # adjust_for_missing adjusts for N values in the p.value column.
  # this doesn't seem quite right to me as a default because no attempt at a
  #.  "discovery" was made.
  if (adjust_for_missing) {
    coef_dat %>%
      mutate(q.value = p.adjust(p = p.value, method = 'fdr', n = n()))
  } else {
    coef_dat %>%
      mutate(q.value = p.adjust(p = p.value, method = 'fdr'))
  }
}
