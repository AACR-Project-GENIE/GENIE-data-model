# Used to declare unquoted variables names in dplyr scripts.
# Most of these are variable names in PRISSMM.
utils::globalVariables("field_name")
utils::globalVariables("letter_code")
utils::globalVariables("required")
utils::globalVariables("col_read_type")
utils::globalVariables("valid_val_struc")
utils::globalVariables(".data")


utils::globalVariables("ca_seq")
utils::globalVariables("dob_ca_dx_days")
utils::globalVariables("record_id")
utils::globalVariables("redcap_repeat_instrument")
utils::globalVariables("redcap_repeat_instance")
utils::globalVariables("redcap_ca_index")
utils::globalVariables("redcap_ca_seq")
utils::globalVariables(".")

utils::globalVariables(
  names = c(
    "dat_dict_sub",
    "form_in_extract",
    "var_list",
    "redcap_repeat_instrument"
  )
)

# Names from the data dictionary:
utils::globalVariables(
  names = c(
    "Variable / Field Name",
    "Form Name",
    "Field Type",
    "Choices, Calculations, OR Slider Labels",
    "Required Field?"
  )
)

utils::globalVariables(
  names = c(
    "stub",
    "num",
    'closest',
    'var',
    '.is_first',
    'valid_val_str',
    'valid_val_key_code',
    'valid_val_value_meaning'
  )
)

utils::globalVariables(
  names = c(
    "naaccr_ethnicity_code",
    "naaccr_race_code_primary",
    "naaccr_race_code_secondary",
    "naaccr_race_code_tertiary",
    "naaccr_sex_code",
    "hybrid_death_source",
    "institution"
  )
)

utils::globalVariables(
  names = c(
    "best_ajcc_stage_cd",
    "ca_stage_iv",
    "ca_stage",
    "stage_dx_iv",
    "stage_dx",
    "ca_age",
    "naaccr_diagnosis_age",
    "age_dx",
    "ca_site",
    "ca_d_site",
    "ca_dx_how",
    "ca_type",
    "ca_clin_t_stage",
    "ca_clin_t1_det",
    "ca_clin_t2_det",
    "ca_clin_t3_det",
    "ca_clin_t4_det",
    "drugs_startdt_int_1",
    "regimen_number",
    ".affected_cancer",
    "drugs_dc_ynu",
    "drugs_enddt_int_1",
    "drugs_enddt_int_2",
    "drugs_enddt_int_3",
    "drugs_enddt_int_4",
    "drugs_enddt_int_5",
    "drugs_lastdt_int_1",
    "drugs_lastdt_int_2",
    "drugs_lastdt_int_3",
    "drugs_lastdt_int_4",
    "drugs_lastdt_int_5"
  )
)
