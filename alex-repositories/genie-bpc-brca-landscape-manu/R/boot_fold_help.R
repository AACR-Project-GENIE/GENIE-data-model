#' @description A bootstrap helper that assigns fold labels to participants.
#' 
#' @details The point of this function is assigning 'copies' of the same record
#'   to the same fold, so that a person does not serves as their own validation.
#'   It returns the bootstrapped data, with one extra column for the fold ID.
#' @param dat A dataframe to bootstrap.
#' @param seed REQUIRED.  Seed for the bootstrap resamples.
#' @param n_folds The number of folds to assign people to.
boot_fold_help <- function(dat, seed, n_folds = 5) {
  
  dat %<>%
    add_id(name = ".orig_id")
  
  set.seed(seed)
  boot_dat <- dat[sample(1:nrow(dat), replace = T), ]
  
  assign_dat <- boot_dat %>% 
    select(.orig_id) %>%
    distinct(.) %>%
    mutate(
      fold = sample(extend_to(1:n_folds, n()))
    )
  
  rtn <- left_join(
    boot_dat,
    assign_dat,
    by = ".orig_id"
  ) %>%
    select(-.orig_id)
  
  return(rtn)
  
}