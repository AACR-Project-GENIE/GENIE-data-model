surv_cohort_track_help <- function(dat, state, var = "bca_subtype_f_simple") {
  dat %>%
    tabyl(all_of(var)) %>%
    adorn_totals(., name = "All") %>%
    select(1:2) %>%
    mutate(
      state = state
    )
}