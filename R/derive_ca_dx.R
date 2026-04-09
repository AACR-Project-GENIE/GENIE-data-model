#' Derive cancer diagnosis form
#'
#' @param tab Cancer diagnosis form from the raw redcap.
#' @param dat_dict_sub The subset of the data dictionary with variables for this instrument.
#'
#' @returns A dataframe similar to ca_dx with additional derivations.
#' @export
#'
#' @examples # Going to need synthetic data probably...
derive_ca_dx <- function(
  tab,
  dat_dict_sub
) {
  raw_ca_dx <- column_exclusion_helper_derived(tab)

  rtn <- common_data_derivation_operations(
    dat = raw_ca_dx,
    dict = dat_dict_sub
  )

  rtn <- rtn %>%
    dplyr::mutate(
      dob_ca_dx_days = dplyr::case_when(
        is.na(ca_cadx_int) ~ naaccr_diagnosis_int,
        T ~ ca_cadx_int
      )
    )

  # Add the record level annotations that are jammed into this data for some reason.
  rtn <- rtn %>%
    dplyr::group_by(record_id) %>%
    dplyr::mutate(
      n_cancers = dplyr::n(),
      n_cancers_index = sum(redcap_ca_index %in% "Yes")
    ) %>%
    dplyr::ungroup(.)

  # rearrange the cancer diagnosis form according to the arcane logic in BPC.
  rtn <- rtn %>%
    dplyr::group_by(record_id) %>%
    # break ties by keeping the order of redcap_ca_seq
    dplyr::arrange(dob_ca_dx_days, redcap_ca_seq) %>%
    dplyr::mutate(
      ca_seq = dplyr::case_when(
        n_cancers %in% 1 ~ 0,
        T ~ 1:dplyr::n()
      )
    ) %>%
    dplyr::ungroup(.)

  rtn <- rtn %>%
    dplyr::relocate(ca_seq, .after = record_id)

  return(rtn)
}
