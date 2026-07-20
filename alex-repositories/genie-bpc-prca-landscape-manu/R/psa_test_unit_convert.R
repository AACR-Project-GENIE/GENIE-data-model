
psa_test_unit_convert <- function(tm_dat) {
  ref <- tribble(
    # cf = conversion factor
    # converting to ng/mL for psa and ng/dL for test.
    ~tm_type, ~tm_result_units, ~cf,
    "PSA", "ng/dL", 0.01,
    "PSA", "ng/mL", 1,
    "PSA", "ug/L", 1,
    
    "Testosterone", "ng/dL", 1,
    "Testosterone", "ng/mL", 100,
    "Testosterone", "nmol/L", 28.81,
    "Testosterone", "pg/mL", 0.1,
    "Testosterone", "ug/L", 100
  )
  
  mod <-left_join(
    tm_dat,
    ref,
    by = c("tm_type", "tm_result_units")
  ) %>%
    mutate(
      tm_num_result_conv = case_when(
        !(tm_type %in% c("PSA", "Testosterone")) ~ tm_num_result,
        is.na(cf) ~ NA_real_,
        T ~ tm_num_result*cf
      ),
      tm_result_units_conv = case_when(
        !(tm_type %in% c("PSA", "Testosterone")) ~ tm_result_units,
        tm_type %in% "PSA" ~ "ng/mL",
        tm_type %in% "Testosterone" ~ "ng/dL"
      )
    ) %>%
    select(-cf)
  
  return(mod)
}
