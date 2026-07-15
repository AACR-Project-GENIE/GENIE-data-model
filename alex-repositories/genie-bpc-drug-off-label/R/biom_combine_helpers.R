

biom_combine_and <- function(
    biom_1,
    biom_2
) {
  # the logic needed is slightly different than biom_1 & biom_2 due to the case
  #   when one is NA and the other is FALSE.
  case_when(
    is.na(biom_1) | is.na(biom_2) ~ NA,
    T ~ biom_1 & biom_2
  )
}

# easy to see all the cases if desired:
# expand_grid(one = c(NA, T, F), two = c(NA, T, F)) %>% mutate(res = biom_combine_and(one, two))

biom_combine_or <- function(
    biom_1,
    biom_2
) {
  # the logic here is perfectly fine because if one is true, it doesn't matter
  #   that the other one wasn't tested.
  # I'm just keeping this as a function for consistency.
  biom_1 | biom_2
}

# expand_grid(one = c(NA, T, F), two = c(NA, T, F)) %>% mutate(res = biom_combine_or(one, two))

