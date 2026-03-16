# Description: Load and reshape the MSK dervied datasets on genes.

library(dplyr)
library(readr)

# Downloaded from https://app.box.com/file/1176682730774
mut_msk <- read.table(
  here(
    "data",
    "msk_box_derived",
    "MUTATION_ONCOKB_ANNOTATED_COMMON_158_GENES_ACROSS_ALL_PANELS.txt"
  ),
  row.names = 1,
  check.names = F
) 

mut_msk <- mut_msk %>% 
  as.matrix(.) %>% 
  t %>% 
  as_tibble(rownames = "stable_id")

readr::write_rds(
  x = mut_msk,
  file = here('data', 'msk_box_derived', 'mut_reshape.rds')
)



# Do the same processing on copy number alterations.
cna_msk <- read.table(
  here(
    "data",
    "msk_box_derived",
    "COPYNUMBER_ONCOKB_ANNOTATED_COMMON_158_GENES_ACROSS_ALL_PANELS.txt"
  ),
  row.names = 1,
  check.names = F
)

cna_msk <- cna_msk %>% 
  as.matrix(.) %>% 
  t %>% 
  as_tibble(rownames = "stable_id")

readr::write_rds(
  x = cna_msk,
  file = here('data', 'msk_box_derived', 'cna_reshape.rds')
)


# Annnnnd one more time for fusions:
fus_msk <- read.table(
  here(
    "data",
    "msk_box_derived",
    "FUSION_ONCOKB_ANNOTATED_COMMON_158_GENES_ACROSS_ALL_PANELS.txt"
  ),
  row.names = 1,
  check.names = F
)

fus_msk <- fus_msk %>% 
  as.matrix(.) %>% 
  t %>% 
  as_tibble(rownames = "stable_id")

readr::write_rds(
  x = fus_msk,
  file = here('data', 'msk_box_derived', 'fus_reshape.rds')
)

