# Can easily add stuff to this as needed later on.
dat_name_to_short <- function(dat_name_long, factorize = T) {
  # go by the data guide order for my sanity:
  fact_order <- c(
    'pt',
    'ca_all',
    'ca_ind',
    'ca_non_ind',
    'reg',
    'rad',
    'path',
    'img',
    'med_onc',
    'cpt',
    'mark'
  )

  rtn <- case_when(
    dat_name_long %in% c('ca_all', 'cancer_diagnosis') ~ 'ca_all',
    dat_name_long %in% c('ca_ind') ~ 'ca_ind',
    dat_name_long %in% c('ca_non_ind') ~ 'ca_non_ind',
    dat_name_long %in% c('reg', 'ca_directed_drugs') ~ 'reg',
    dat_name_long %in% c('rad', 'ca_directed_radtx') ~ 'rad',
    dat_name_long %in% c('cpt', 'cancer_panel_test') ~ 'cpt',
    dat_name_long %in% c('pt', 'patient') ~ 'pt',
    dat_name_long %in% c('img', 'prissmm_imaging') ~ 'img',
    dat_name_long %in% c('med_onc', 'prissmm_med_onc_assessment') ~ 'med_onc',
    dat_name_long %in% c('path', 'prissmm_pathology') ~ 'path',
    dat_name_long %in% c('mark', 'prissmm_tumor_marker') ~ 'mark'
  )

  if (factorize) {
    factor(rtn, levels = fact_order)
  } else {
    rtn
  }
}
