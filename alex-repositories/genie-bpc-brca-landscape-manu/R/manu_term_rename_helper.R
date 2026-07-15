manu_term_rename_helper <- function(vec) {
  if (is.factor(vec)) {
    recode_levs <- c(
      ERBB2_amp = "ERBB2_cna",
      de_novo_met = "stage_dx_iv_num",
      her2_pos = "bca_her2_pos",
      TNBC = "bca_trip_neg",
      receptor_unk = "bca_nc_nr"
    )
    # remove any not found in input vector
    recode_levs <- recode_levs[recode_levs %in% vec]
    
    vec <- forcats::fct_recode(vec, !!!recode_levs) 
  } else { 
    vec <- case_when(
      vec %in% "ERBB2_cna" ~ 'ERBB2_amp',
      vec %in% "stage_dx_iv_num" ~ "de_novo_met",
      vec %in% "bca_her2_pos" ~ "her2_pos",
      vec %in% "bca_trip_neg" ~ "bca_trip_neg",
      vec %in% "bca_nc_nr" ~ "receptor_unk",
      T ~ vec
    )
  }
  return(vec)
}
