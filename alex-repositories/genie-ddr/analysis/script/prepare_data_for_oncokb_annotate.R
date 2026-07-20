filter_to_high_amplifications_only <- T
downsample_for_testing <- F

library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

path_geno <- here('data-raw', 'main_genie')

mut <- fread(
  here(path_geno, 'data_mutations_extended.txt')
)
cna <- fread(
  here(path_geno, "data_CNA.txt")
)
sv <- fread(
  file = here(path_geno, 'data_sv.txt')
)

mut %<>% as_tibble(.)
cna %<>% as_tibble(.)
sv %<>% as_tibble(.)

# Using the slower read.tsv here to leverage the comment option.
samp <- read_tsv(
  file = here(path_geno, 'data_clinical_sample.txt'),
  comment = '#'
)


# The CNA file needs to be reshaped to be fed into the annotator the way I know how:
cna_long_selected <- cna %>%
  pivot_longer(
    cols = -Hugo_Symbol,
    names_to = "Tumor_Sample_Barcode", # just to match.
    values_to = "value"
  )

if (filter_to_high_amplifications_only) {
  # When people refer to copy-number alterations they often mean the high level amplifications.
  # The only real reason to do this at this stage is saving
  #   time with the annotator script.
  cna_long_selected <- cna_long_selected %>%
    filter(!is.na(value) & value >= 2)
}

# For each of our three file types we will add in the oncotree code:
dft_otc <- samp %>%
  select(
    Tumor_Sample_Barcode = SAMPLE_ID,
    ONCOTREE_CODE
  )

sv %<>% rename(Tumor_Sample_Barcode = Sample_Id)

mut <- left_join(
  mut,
  dft_otc,
  by = c('Tumor_Sample_Barcode'),
  relationship = "many-to-one"
)
cna_long_selected <- left_join(
  cna_long_selected,
  dft_otc,
  by = c('Tumor_Sample_Barcode'),
  relationship = "many-to-one"
)
sv <- left_join(
  sv,
  dft_otc,
  by = c('Tumor_Sample_Barcode'),
  relationship = "many-to-one"
)

# Spits out a message for each dataframe about the number of rows removed
#  and returns the data with the rows removed.

# mut <- genomic_row_removed_helper(mut)
# cna_long_selected <- genomic_row_removed_helper(cna_long_selected)
# sv <- genomic_row_removed_helper(sv)

if (downsample_for_testing) {
  # choices here are totally heuristic.
  # mutations has about 2 million rows, 2k is enough to test.
  mut %<>% sample_frac(0.001)
  # for cna/fus about 1% is a better range.
  cna_long_selected %<>% sample_frac(0.01)
  sv %<>% sample_frac(0.01)
}


readr::write_tsv(
  x = mut,
  file = here('data', 'genomic', 'mut_ready_to_annotate.txt'),
  na = ''
)
readr::write_tsv(
  x = cna_long_selected,
  file = here('data', 'genomic', 'cna_ready_to_annotate.txt'),
  na = ''
)
readr::write_tsv(
  x = sv,
  file = here('data', 'genomic', 'fus_ready_to_annotate.txt'),
  na = ''
)
