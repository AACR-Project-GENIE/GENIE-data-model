library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

qc_res_dir <- path(qc_config$storage_root, 'result')

qc_res <- pull_all_results(qc_res_dir)

# For now all the datasets
# qc_test <- qc_res %>% filter(dat_name %in% "cancer_diagnosis")
# iss_test <- list_issues(
#   val_sub = qc_test$validation_subset[[1]],
#   extracts = qc_test$extracts[[1]],
#   report = qc_test$report[[1]]
# )

qc_res %<>%
  mutate(
    issues = purrr::pmap(
      .l = list(
        val_sub = validation_subset,
        extracts = extracts,
        report = report
      ),
      .f = list_issues
    )
  )

# some manual fixing of output types here:
qc_res %<>%
  mutate(
    issues = purrr::map(
      .x = issues,
      .f = \(x) {
        if (is.null(x)) {
          x
        } else {
          x %>%
            mutate(
              redcap_repeat_instance = as.numeric(redcap_repeat_instance)
            )
        }
      }
    )
  )

issues_list <- qc_res %>%
  select(qc_layer, dat_name, issues) %>%
  unnest(issues)

issues_list %<>%
  select(
    record_id,
    redcap_repeat_instrument,
    redcap_repeat_instance,
    any_of('observed_value'),
    brief,
    label,
    columns,
    assertion_type,
    asserted_values = values,
    columns,
    dat_name,
    qc_layer,
    date_generated = time_processed
  )

issues_list %<>%
  mutate(
    date_generated = as_date(date_generated),
    date_generated = format(date_generated, '%Y-%b-%d')
  )

dir_out <- path(qc_config$storage_root, 'output')

# take the last folder name in the path to help track versions later:
last_folder_name <- str_extract(qc_config$storage_root, '[^\\/]*$')

readr::write_excel_csv(
  issues_list,
  # could add the date or something if desired for uniqueness.
  path(
    qc_config$storage_root,
    'output',
    paste0(last_folder_name, '_issues_internal.csv')
  ),
  na = ""
)

# Additionally we'll want a list of all the tests tat were done:
tests_run <- qc_res %>%
  select(qc_layer, site, dat_name, validation_subset) %>%
  unnest(validation_subset)

readr::write_excel_csv(
  tests_run,
  path(
    qc_config$storage_root,
    'output',
    paste0(last_folder_name, '_tests_run.csv')
  ),
  na = ""
)

# make a copy of the excel template with a proper name and place.
template_path <- path(
  qc_config$storage_root,
  'output',
  paste0(last_folder_name, '_issues.xlsx')
)
if (!fs::file_exists(template_path)) {
  fs::file_copy(
    path = here('data-raw', 'qc_site_template.xlsx'),
    new_path = template_path
  )
} else {
  # designed to avoid overwritting manual edits that take time.
  cli_warn("Issues template exists already - skipping paste.")
}

# Produce an issue count for each subject
issues_list %>%
  count(record_id, name = "issue_count") %>%
  readr::write_excel_csv(
    .,
    path(
      qc_config$storage_root,
      'output',
      paste0(last_folder_name, '_issue_counts.csv')
    ),
    na = ""
  )
