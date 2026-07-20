fix_cohort_names <- function(dat) {
  dat %>%
    mutate(
      cohort = case_when(
        cohort %in% "BLADDER" ~ "Bladder",
        cohort %in% "BrCa" ~ "Breast",
        cohort %in% "PANC" ~ "Pancreas",
        T ~ cohort
      )
    )
}