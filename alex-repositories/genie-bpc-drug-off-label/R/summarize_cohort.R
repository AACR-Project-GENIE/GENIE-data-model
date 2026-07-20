# cdw = clin_dat_wide - a dataset of datasets.
summarize_cohort <- function(cdw) {
  rtn <- cdw %>%
    mutate(
      n_pts = purrr::map_dbl(
        .x = pt,
        .f = nrow
      ),
      n_cancers = purrr::map2_dbl(
        .x = ca_ind,
        .y = ca_non_ind,
        .f = \(x,y) {
          nrow(x) + nrow(y)
        }
      ),
      n_reg = purrr::map_dbl(
        .x = hreg,
        .f = nrow
      ),
      n_unique_drug = purrr::map_dbl(
        .x = hdrug,
        .f = \(x) {
          pull(x,agent) %>% unique %>% length
        }
      )
    )
  
  rtn %>%
    select(cohort, matches("^n_")) %>%
    return(.)
}