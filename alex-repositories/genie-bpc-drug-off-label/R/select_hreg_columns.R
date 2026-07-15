#' @title Select the columns to make a harmonized regimen dataset.
select_hreg_columns <- function(
    dat_reg 
) {
  
  # Select only the columns we have consistently across all (after fixes)
  hreg_req_col <- c(
    # The ones I actually want:
    'cohort',
    'record_id',
    'institution',
    'ca_seq',
    'regimen_number',
    'redcap_ca_index',
    'drugs_num',
    "drugs_ct_yn",
    "regimen_drugs",
    "drugs_drug_1", 
    "drugs_drug_2", 
    "drugs_drug_3",
    "drugs_drug_4", 
    "drugs_drug_5",
    
    # dob to start
    "drugs_startdt_int_1", 
    "drugs_startdt_int_2", 
    "drugs_startdt_int_3", 
    "drugs_startdt_int_4", 
    "drugs_startdt_int_5", 
    
    # dx to start 
    "dx_drug_start_int_1", 
    "dx_drug_start_int_2", 
    "dx_drug_start_int_3", 
    "dx_drug_start_int_4", 
    "dx_drug_start_int_5",
    
    # dx to end if dc'd
    "dx_drug_end_int_1", 
    "dx_drug_end_int_2", 
    "dx_drug_end_int_3", 
    "dx_drug_end_int_4", 
    "dx_drug_end_int_5",
    
    # dx to (end OR last known admin)
    "dx_drug_end_or_lastadm_int_1", 
    "dx_drug_end_or_lastadm_int_2", 
    "dx_drug_end_or_lastadm_int_3", 
    "dx_drug_end_or_lastadm_int_4", 
    "dx_drug_end_or_lastadm_int_5",
    
    "dx_reg_start_int",
    "dx_reg_end_any_int", # dx to end of first drug
    "dx_reg_end_all_int",
    
    "os_d_status",
    "tt_os_d1_days", 
    "tt_os_d2_days", 
    "tt_os_d3_days", 
    "tt_os_d4_days", 
    "tt_os_d5_days",
    
    "os_g_status", # g = ... regimen? can't figure that one out.
    "tt_os_g_days",
    
    "pfs_i_and_m_g_status",
    "tt_pfs_i_and_m_g_days"
  )
  
  rtn <- select(dat_reg, all_of(hreg_req_col)) %>%
      arrange(record_id, ca_seq, regimen_number)
  
  return(rtn)
  
}