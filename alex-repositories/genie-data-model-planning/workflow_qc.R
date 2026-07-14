# Main workflow for the project.

library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

script_runner <- function(
  name
) {
  source(here('analysis', 'script', name))
  invisible(name)
}

# Config file loading:
# qc_conf_file <- 'her2_dfci_config.yml'
# qc_conf_file <- 'her2_msk_config.yml'
# qc_conf_file <- 'her2_colu_config.yml'
# qc_conf_file <- 'her2_ucsf_config.yml'
qc_conf_file <- 'her2_prov_config.yml'

qc_config <- read_config_file(here('data-raw', 'qc_config_files', qc_conf_file))
storage_setup(store_dir = qc_config$storage_root)


script_runner('get_raw_data_qc.R')

# Different workflow not requiring the release analysis:
script_runner('align_data_dict.R')
script_runner('qc_l0_raw_redcap.R')

script_runner('split_redcap_into_tables.R')

# Probably add a check here that the splitting went OK.

# Layer 1 checks:
script_runner('qc_l1_patient.R')
script_runner('qc_l1_cancer_diagnosis.R')
script_runner('qc_l1_ca_directed_drugs.R')
script_runner('qc_l1_ca_directed_radtx.R')
script_runner('qc_l1_prissmm_pathology.R')
script_runner('qc_l1_prissmm_imaging.R')
script_runner('qc_l1_prissmm_med_onc_assessment.R')
# Don't think this exsists for this project:
# script_runner('qc_l1_prissmm_tumor_marker.R')
script_runner('qc_l1_cancer_panel_test.R')

# For now I'm just going to look manually here:
layer_one_sum <- display_results_summary(path(qc_config$storage_root, 'result'))
cli::cli_progress_message("Summary after QC layer 1:")
print(layer_one_sum)
if (any(!layer_one_sum$all_passed)) {
  script_runner('summarize_testing.R')
  cli::cli_abort("Summarizing and stopping at QC layer 1, errors found.")
} else {
  cli::cli_alert_success("QC layer 1 passed.")
}


# Create the layer 2 datasets:
script_runner('layer_2_data_creation.R')

# It's valid but probably not common to add level 2 dataset issues if needed.

script_runner('qc_l3_cpt_LJ_ca_all.R')
script_runner('qc_l3_rad_LJ_ca_ind.R')
script_runner('qc_l3_reg_LJ_ca_all.R')
script_runner('qc_l3_pt_FJ_ca_ind.R')
script_runner('qc_l3_reg_LJ_pt.R')

layer_three_sum <- display_results_summary(path(
  qc_config$storage_root,
  'result'
))
cli::cli_progress_message("Summary after QC layer 3:")
print(layer_three_sum)
if (any(!layer_three_sum$all_passed)) {
  script_runner('summarize_testing.R')
  cli::cli_abort("Errors found in QC layer 3 - see issue summary.")
} else {
  cli::cli_alert_success("No issues found!")
}
