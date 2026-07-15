
library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source) #

read_wrap_clin <- function(p) {
  read_rds(file = here("data", 'clin', p))
}
dft_cpt <- read_wrap_clin("dft_cpt_aug.rds")

read_wrap_geno <- function(p) {
  read_rds(file = here("data", 'genomic', p))
}
dft_alt <- read_wrap_geno('alterations.rds')

bed <- readr::read_tsv(
  here('data-raw', 'genomic', 'genie_combined.bed')
)

genes_with_all_excluded <- bed %>%
  filter(str_detect(SEQ_ASSAY_ID, "DFCI-ONCOP")) %>%
  group_by(SEQ_ASSAY_ID, Hugo_Symbol) %>%
  summarize(
    prop_included = mean(includeInPanel),
    .groups = "drop"
  ) %>%
  filter(prop_included %in% 0) %>%
  rename(
    cpt_seq_assay_id = SEQ_ASSAY_ID,
    hugo = Hugo_Symbol
  )

dft_alt <- dft_cpt %>%
  select(
    sample_id = cpt_genie_sample_id,
    cpt_seq_assay_id
  ) %>%
  left_join(
    dft_alt,
    .,
    by = "sample_id"
  )

inner_join(
  genes_with_all_excluded,
  filter(dft_alt, alt_type %in% "Mutation"),
  by = c("cpt_seq_assay_id", "hugo")
) %>%
  select(sample_id, hugo, hgvsc, hgvsp, cpt_seq_assay_id) %>%
  readr::write_tsv(
    x = .,
    file = here('analysis', 'explore', 'DFCI_BPC_prostate_excluded_genes_but_reported_anyway.txt')
  )

bed %>% 
  filter(SEQ_ASSAY_ID %in% "DFCI-ONCOPANEL-3") %>%
  filter(Hugo_Symbol %in% "BIRC3") %>%
  pull(includeInPanel)
