is_drug_in_regimens <- function(drug_str, reg_dat) {
  any(
    str_detect(
      reg_dat,
      drug_str
    )
  )
}

# is_drug_in_regimens(
#   dft_drug_map$agent[1],
#   data_list$BrCa_v1.2$ca_drugs$regimen_drugs
# )
# Examples of one that should fail:
# is_drug_in_regimens(
#   "Abemaciclibileebilbeeboo", # nonsense drug.
#   data_list$BrCa_v1.2$ca_drugs$regimen_drugs
# )