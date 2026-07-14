# A very limited scope function that just takes a column called "file" and adds
#.  one called "table".  Designed for the file names used in releases.
file_2_short <- function(
  dat
) {
  ref_tab <- tribble(
    ~file,
    ~table,
    'ca_radtx_dataset.csv',
    'rad',

    'cancer_level_dataset_index.csv',
    'ca_index',

    'cancer_level_dataset_non_index.csv',
    'ca_non_index',

    'cancer_panel_test_level_dataset.csv',
    'cpt',

    'imaging_level_dataset.csv',
    'img',

    'med_onc_note_level_dataset.csv',
    'med_onc',

    'pathology_report_level_dataset.csv',
    'path',

    'patient_level_dataset.csv',
    'pt',

    'regimen_cancer_level_dataset.csv',
    'reg',

    'tm_level_dataset.csv',
    'tm'
  )

  left_join(
    dat,
    ref_tab,
    by = 'file'
  ) %>%
    relocate(table, .after = file)
}
