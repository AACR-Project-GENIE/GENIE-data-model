basic_line_of_therapy <- function(
  start_counting_df,
  reg,
  ignored_regimen_drugs = NULL,
  remove_duplicates = T,
  verbose = F
) {
  if (is.null(ignored_regimen_drugs)) {
    # default is the LHRH agonists and select others.
    ignored_regimen_drugs <- basic_line_ignored_drugs()
  }

  reg %<>%
    filter(!(regimen_drugs %in% ignored_regimen_drugs))

  reg <- reg |>
    mutate(
      # creating the analogous version of dx_reg_start_int...
      dob_reg_start_int = pmin(
        drugs_startdt_int_1,
        drugs_startdt_int_2,
        drugs_startdt_int_3,
        drugs_startdt_int_4,
        drugs_startdt_int_5,
        na.rm = T
      )
    )

  line_of_ther <- left_join(
    reg,
    start_counting_df,
    by = c('record_id', 'ca_seq'),
    relationship = 'many-to-one'
  )

  line_of_ther %<>%
    mutate(
      post_met_reg = case_when(
        is.na(dob_start_days) ~ F,
        T ~ dob_reg_start_int >= dob_start_days - 0.5
      )
    )

  n_before <- nrow(line_of_ther)

  line_of_ther %<>%
    filter(post_met_reg)

  if (remove_duplicates) {
    # This is crude - the end dates are not fixed we just remove the second
    #   regimen if it's the same as the previous one.
    line_of_ther %<>%
      group_by(record_id) %>%
      arrange(dx_reg_start_int) %>%
      mutate(
        .prev_reg_drugs = lag(regimen_drugs),
        .dup_reg = case_when(
          is.na(.prev_reg_drugs) ~ F, # can't check, should be first row.
          .prev_reg_drugs == regimen_drugs ~ T,
          T ~ F
        )
      ) %>%
      ungroup(.) %>%
      filter(!.dup_reg) %>%
      select(-c(.prev_reg_drugs, .dup_reg))
  }

  n_after <- nrow(line_of_ther)

  if (verbose) {
    cli::cli_inform(
      "Out of {n_before} regimens {n_after} were eligible to be considered lines of therapy"
    )
  }

  line_of_ther %<>%
    group_by(record_id, ca_seq) %>%
    arrange(dx_reg_start_int) %>%
    mutate(line_of_therapy = 1:n()) %>%
    ungroup(.)

  line_of_ther %<>%
    select(
      record_id,
      ca_seq,
      regimen_number,
      line_of_therapy,
      regimen_drugs,
      post_met_reg,
      dx_reg_start_int,
      dob_reg_start_int
    )

  return(line_of_ther)
}

basic_line_ignored_drugs <- function() {
  c(
    'Leuprolide Acetate',
    'Goserlin Acetate',
    'Triptorelin',
    'Histrelin Acetate',
    'BCG Solution',
    'BCG Vaccine'
  )
}
