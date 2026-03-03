dd_assign_coltypes <- function(
  dd,
  dttm_cols = NULL,
  date_cols = NULL,
  num_cols = NULL
) {
  if (is.null(dttm_cols)) {
    dttm_cols <- c('pt', 'ca', 'drugs', 'rt', 'path', 'image', 'md')
    dttm_cols <- c(
      paste0(dttm_cols, '_start_time'),
      paste0(dttm_cols, '_stop_time')
    )
  }

  date_cols <- date_cols %||% c('cpt_seq_date', 'qa_full_date')

  if (is.null(num_cols)) {
    num_cols <- c(
      'redcap_ca_seq',
      # all the interval columns:
      "hybrid_death_int",
      "last_oncvisit_int",
      "last_alive_int",
      "last_anyvisit_int",
      "enroll_hospice_int",
      "naaccr_diagnosis_int",
      "naaccr_first_contact_int",
      "ca_cadx_int",
      "rt_start_int",
      "rt_end_int",
      "rt_rt_int",
      "path_rep_int",
      "path_proc_int",
      "path_erprher_add1_int",
      "path_erprher_add2_int",
      "path_erprher_add3_int",
      "path_erprher_add4_int",
      "path_erprher_add5_int",
      "image_scan_int",
      "image_report_int",
      "image_ref_scan_int",
      "md_onc_visit_int",
      "tm_spec_collect_int",
      "cpt_order_int",
      "cpt_report_int",

      # and a few others:
      'birth_year',
      'rt_dose',
      'rt_total_dose'
    )
  }

  dd <- dd %>%
    dplyr::mutate(
      col_read_type = dplyr::case_when(
        # The NAs are an odd bunch.  Some of the drug stuff is NA but it's mostly
        #   the "complete" fields, which seem to use 0/1/2 coding.
        is.na(field_type) ~ 'char',
        # This group is fundamentally categorical, but almost all are integers.  Could be read in as numeric or character reasonably.
        field_type %in% c('checkbox', 'dropdown', 'radio', 'complete_check') ~
          'char',
        # This group is not categorical.
        field_type %in% c('text') & field_name %in% num_cols ~ 'numeric',
        field_type %in% c('text') & field_name %in% date_cols ~ 'date',
        field_type %in% c('text') & field_name %in% dttm_cols ~ 'dttm',
        field_type %in% c('text') ~ 'char',
        # it's a further manipulation to go to T/F from y/n, so we'll leave that for now:
        field_type %in% 'yesno' ~ 'char'
      )
    )

  dd
}
