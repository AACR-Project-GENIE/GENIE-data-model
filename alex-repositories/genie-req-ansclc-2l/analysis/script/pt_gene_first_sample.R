library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

samp <- read_tsv(
  here('data-raw', 'main_genie', 'data_clinical_sample.txt'),
  comment = '#'
) %>%
  rename_all(tolower)

mut_mat <- readr::read_rds(
  here('data', 'genomic', 'mut_mat_lim.rds')
)

pt_gene <- mut_mat %>%
  as_tibble(rownames = "sample_id") %>%
  left_join(
    .,
    select(samp, sample_id, patient_id),
    by = 'sample_id'
  )

pt_gene %>%
  select(-sample_id) %>%
  group_by(patient_id) %>%
  summarize(
    across(
      .cols = everything(),
      # The %in% TRUE part makes NA values into FALSE.
      .fns = ~ max(.x %in% TRUE, na.rm = T)
    )
  )

cli_abort("Need to get KRAS G12C not just KRAS")
