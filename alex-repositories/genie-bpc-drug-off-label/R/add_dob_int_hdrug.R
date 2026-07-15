add_dob_int_hdrug <- function(
    dat_hdrug
) {
  dat_hdrug %>%
    mutate(
      dob_drug_start_int = drugs_startdt_int,
      .start_end_int = dx_drug_end_or_lastadm_int - dx_drug_start_int,
      dob_drug_end_or_lastadm_int = drugs_startdt_int + .start_end_int
    ) %>%
    select(-.start_end_int)
}