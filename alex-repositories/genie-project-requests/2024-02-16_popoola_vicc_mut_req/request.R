# Request:  Filter mutations from two releases down to VICC only and save as 
#  xlsx.

library(fs); library(purrr); library(here);
purrr::walk(fs::dir_ls(here('R')), .f = source)

synLogin()

dft_mut_v11 <- get_synapse_entity_txt(synapse_id = "syn9734426", version = 91)
dft_mut_v15 <- get_synapse_entity_txt(synapse_id = "syn9734426")

dft_mut_v11 %<>% filter(center %in% "VICC")
dft_mut_v15 %<>% filter(center %in% "VICC")


fs::dir_create('output')
readr::write_tsv(
  x = dft_mut_v11, na = "",
  file = here('output', 'data_mutations_extended_VICC_11-public.txt')
)
readr::write_tsv(
  x = dft_mut_v15, na = "",
  file = here('output', 'data_mutations_extended_VICC_15-public.txt')
)

writexl::write_xlsx(
  x = dft_mut_v11, 
  path = here('output', 'data_mutations_extended_VICC_11-public.xlsx')
)
writexl::write_xlsx(
  x = dft_mut_v15, 
  path = here('output', 'data_mutations_extended_VICC_15-public.xlsx')
)

