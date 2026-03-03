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
utils::globalVariables("redcap_ca_index")
utils::globalVariables("redcap_ca_seq")
utils::globalVariables(".")

utils::globalVariables("dat_dict_sub")
utils::globalVariables("form_in_extract")
utils::globalVariables("var_list")
utils::globalVariables("redcap_repeat_instrument")

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
    "num"
  )
)
