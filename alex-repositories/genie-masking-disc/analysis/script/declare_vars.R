library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_to_dx_vars <- tribble(
  ~var, ~unit,
  "age_dx", "year",
  "ca_age", "year",
  "ca_cadx_int", "day",
  
  "dob_ca_dx", "day",
  "dob_ca_dx_mos", "month",
  "dob_ca_dx_yrs", "year"
)


# Note:  In BPC the following datasets have the ca_seq variable as a key:
#   ca_ind, ca_non_ind, cpt, reg
# And these datasets do not have it:
#   img, path, pt, tm
# We'll use the first set and disregard (comment out below) any variables that
#   do not have ca_seq.  This is because they index from the first BPC project
#   cancer which is not as immediately linkable (it's doable, but not worth
#   it to me).

dft_from_dx_vars <- tribble(
  ~var, ~unit,
  'dx_death_int', 'day',
  'dx_death_int_mos', 'month',
  'dx_death_int_yrs', 'year',
  
  'dx_to_dmets_abdomen_days', 'day',
  'dx_to_dmets_bone_days', 'day',
  'dx_to_dmets_brain_days', 'day',
  'dx_to_dmets_breast_days', 'day',
  'dx_to_dmets_days', 'day',
  'dx_to_dmets_extremity_days', 'day',
  'dx_to_dmets_head_neck_days', 'day',
  'dx_to_dmets_heme_days', 'day',
  'dx_to_dmets_liver_days', 'day',
  'dx_to_dmets_pelvis_days', 'day',
  'dx_to_dmets_trunk_days', 'day',
  
  'dx_to_dmets_abdomen_mos', 'month',
  'dx_to_dmets_bone_mos', 'month',
  'dx_to_dmets_brain_mos', 'month',
  'dx_to_dmets_breast_mos', 'month',
  'dx_to_dmets_mos', 'month',
  'dx_to_dmets_extremity_mos', 'month',
  'dx_to_dmets_head_neck_mos', 'month',
  'dx_to_dmets_heme_mos', 'month',
  'dx_to_dmets_liver_mos', 'month',
  'dx_to_dmets_pelvis_mos', 'month',
  'dx_to_dmets_trunk_mos', 'month',
  
  'dx_to_dmets_abdomen_yrs', 'year',
  'dx_to_dmets_bone_yrs', 'year',
  'dx_to_dmets_brain_yrs', 'year',
  'dx_to_dmets_breast_yrs', 'year',
  'dx_to_dmets_yrs', 'year',
  'dx_to_dmets_extremity_yrs', 'year',
  'dx_to_dmets_head_neck_yrs', 'year',
  'dx_to_dmets_heme_yrs', 'year',
  'dx_to_dmets_liver_yrs', 'year',
  'dx_to_dmets_pelvis_yrs', 'year',
  'dx_to_dmets_trunk_yrs', 'year',
  
  'tt_os_dx_days', 'day',
  'tt_os_dx_mos', 'month',
  'tt_os_dx_yrs', 'year',
  
  # These are from date of birth:
  # 'cpt_order_int', 'day',
  # 'cpt_report_int', 'day',
  
  'dx_cpt_rep_days', 'day',
  'dx_cpt_rep_mos', 'month',
  'dx_cpt_rep_yrs', 'year',
  
  # 'dx_path_proc_cpt_days', 'day',
  # 'dx_path_proc_cpt_mos', 'month',
  # 'dx_path_proc_cpt_yrs', 'year',
  
  # 'dx_ref_scan_days', 'day',
  # 'dx_ref_scan_mos', 'month',
  # 'dx_ref_scan_yrs', 'year',
  
  # 'dx_scan_days', 'day',
  # 'dx_scan_mos', 'month',
  # 'dx_scan_yrs', 'year',
  
  # 'dx_md_visit_days', 'day',
  # 'dx_md_visit_mos', 'month',
  # 'dx_md_visit_yrs', 'year',
  
  # 'dx_path_proc_days', 'day',
  # 'dx_path_proc_mos', 'month',
  # 'dx_path_proc_yrs', 'year'
  
  'dx_drug_end_or_lastadm_int_1', 'day',
  'dx_drug_end_or_lastadm_int_2', 'day',
  'dx_drug_end_or_lastadm_int_3', 'day',
  'dx_drug_end_or_lastadm_int_4', 'day',
  'dx_drug_end_or_lastadm_int_5', 'day',
  
  'dx_reg_end_all_int', 'day'
  
)

readr::write_rds(
  dft_to_dx_vars,
  here('data', 'var_to_dx.rds')
)

readr::write_rds(
  dft_from_dx_vars,
  here('data', 'var_from_dx.rds')
)