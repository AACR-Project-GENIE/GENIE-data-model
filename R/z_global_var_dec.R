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
    "drugs_startdt_int_2",
    "drugs_startdt_int_3",
    "drugs_startdt_int_4",
    "drugs_startdt_int_5",
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
    "drugs_lastdt_int_5",
    "drugs_drug_1",
    "drugs_drug_2",
    "drugs_drug_3",
    "drugs_drug_4",
    "drugs_drug_5",
    "dob_reg_start_int",
    "dob_reg_end_any_int",
    "dob_reg_end_all_int",
    "drugs_num",
    "drugs_inst",
    "drugs_firstinst",
    "ca_clin_n_stage",
    "ca_path_group_stage",
    "ca_path_t_stage",
    "ca_path_t1_det",
    "ca_path_t2_det",
    "ca_path_t3_det",
    "ca_path_t4_det",
    "ca_path_n_stage",
    "ca_tx_pre_path_stage",
    "age_at_seq_report",
    "cpt_report_int",
    "dob_cpt_report_days",
    "cpt_number",
    "dx_cpt_rep_days",
    "ca_clin_group_stage",
    "ca_dmets_yn",
    "image_scan_int",
    "scan_number",
    "image_inst_perf",
    "image_inst_inter",
    "image_scan_type",
    "image_ca",
    "image_overall",
    "ca_cadx_int",
    "ca_seq",
    "casite_label",
    "icdo3_site",
    "classification",
    "mets_site_group",
    "numeric_code",
    "mets_in_group",
    "specimen_number",
    "specimen_site",
    "path_ca_flag",
    "path_site_label",
    "ca_dmets_yn",
    "dmets_label",
    "mets_number",
    "stage_dx",
    "path_proc_int",
    "path_proc_number",
    "path_rep_number",
    "path_proc_type",
    "path_proc",
    "path_proc_margins"
  )
)
