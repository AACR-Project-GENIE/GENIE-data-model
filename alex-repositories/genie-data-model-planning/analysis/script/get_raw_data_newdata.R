library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

synLogin()

cur_saver_newdata <- function(
  synid,
  qc_configuration
) {
  cur_dir <- fs::path(
    'data-raw',
    'newly_curated_data',
    qc_configuration$inst
  )

  fs::dir_create(cur_dir)
  ent <- synGet(
    entity = synid,
    downloadLocation = cur_dir,
    ifcollision = "overwrite.local"
  )
  fs::file_move(path = ent$path, new_path = path(cur_dir, 'redcap_data.csv'))
}

dd_saver_newdata <- function(
  synid,
  qc_configuration
) {
  cur_dir <- fs::path(
    'data-raw',
    'newly_curated_data',
    qc_configuration$inst
  )

  print(cur_dir)

  fs::dir_create(cur_dir)
  ent <- synGet(
    entity = synid,
    downloadLocation = cur_dir,
    ifcollision = "overwrite.local"
  )
  fs::file_move(path = ent$path, new_path = path(cur_dir, 'data_dict.csv'))
}

config_list <- c(
  'her2_dfci_config.yml',
  'her2_msk_config.yml',
  'her2_colu_config.yml',
  'her2_ucsf_config.yml',
  'her2_prov_config.yml'
)

for (this_yam in config_list) {
  qc_config <- read_config_file(here(
    'data-raw',
    'qc_config_files',
    this_yam
  ))

  print(qc_config)

  data_file_synid <-
    if (!is.null(qc_config$syn_data$syn_id_inst_file)) {
      qc_config$syn_data$syn_id_inst_file
    } else {
      syn_proj_files <- get_syn_children_df(qc_config$syn_id_inst_folder)
      if (nrow(syn_proj_files) > 1) {
        cli_alert_danger("More than one file in the folder - check this.")
      }
      syn_proj_files$id
    }

  cli::cli_alert_info(
    "File for {qc_config$inst} last modified on {synGet(data_file_synid)$properties$modifiedOn}"
  )

  dd_saver_newdata(
    synid = qc_config$syn_id_data_dict,
    qc_configuration = qc_config
  )

  cur_saver_newdata(
    synid = data_file_synid,
    qc_configuration = qc_config
  )
}
