
get_overlaps_one_row <- function(
    row_to_check,
    dat_overlaps,
    return_dat = F # returns a vector of overlapping drugs if false
) {
  if (nrow(row_to_check) != 1) {
    cli_abort("row_to_check must be one row")
  }
  
  if (!all(c('dob_drug_start_int', 'dob_drug_end_or_lastadm_int') %in% names(row_to_check))) {
    cli_abort("row_to_check must have dob_drug_start_int and dob_drug_end_or_lastadm_int columns")
  }
  
  # Get drugs from this person, except the exact one in row_to_check.
  dat_overlaps %<>%
    filter(
      cohort %in% row_to_check$cohort,
      record_id %in% row_to_check$record_id
    ) %>%
    filter(
      !(
        regimen_number %in% row_to_check$regimen_number &
          drug_number %in% row_to_check$drug_number 
      )
    )
  # another way to do this would be drug name, which we'll check on the back end.
  
  dat_overlaps %<>%
    # as a shorthand I'll use a1 and a2 to indicate the start and end 
    #   of the row dates.
    # b1 and b2 will be the dates in the overlaps data (to be tested for overlap with the row)
    mutate(
      .a1 = row_to_check$dob_drug_start_int,
      .a2 = row_to_check$dob_drug_end_or_lastadm_int,
      .b1 = dob_drug_start_int,
      .b2 = dob_drug_end_or_lastadm_int,
      # cond variables are conditions where we would consider there to be 
      #  an overlap.  The design goal is finding things that definitely overlap.
      .cond1 = (.b1 <= .a1) & (.b2 > .a1),
      .cond2 = (.b2 >= .a2) & (.b1 < .a2),
      .cond3 = (.b1 >= .a1) & (.b2 <= .a2),
      .overlap_exists = .cond1 | .cond2 | .cond3
    ) 
  
  dat_overlaps %<>%
    filter(.overlap_exists) %>%
    select(-matches("^\\."))
  
  if (return_dat) {
    return(dat_overlaps)
  } else {
    rtn <- sort(unique(dat_overlaps$agent))
    return(rtn)
  }
}
