filter_to_high_amplifications_only <- T
downsample_for_testing <- F

library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

path_geno <- here('data-raw', 'main_genie')

# designed because I got different oncokb annotations.
mut <- read_rds(
  here('analysis', 'explore', 'unreproducible_variant_sample.rds')
)

mut %<>% as_tibble(.)

# Using the slower read.tsv here to leverage the comment option.
samp <- read_tsv(
  file = here(path_geno, 'data_clinical_sample.txt'),
  comment = '#'
)


# For each of our three file types we will add in the oncotree code:
dft_otc <- samp %>%
  select(
    Tumor_Sample_Barcode = SAMPLE_ID,
    ONCOTREE_CODE
  )

mut %<>% 
  select(Hugo_Symbol:mutationInCis_Flag)

orig_mut_size <- nrow(mut)
mut %<>%
  slice(rep(1:n(), each = 50)) 

s_weights <- samp %>% count(ONCOTREE_CODE) %>%
  mutate(w = n/sum(n))
rand_oncotrees <- sample(
  s_weights$ONCOTREE_CODE, size = 49, prob = s_weights$w
)
rand_oncotrees <- c(rand_oncotrees, "UNKNOWN") # def want to look at unknown too.

mut %<>%
  mutate(
    ONCOTREE_CODE = rep(rand_oncotrees, times = orig_mut_size)
  )


readr::write_tsv(
  x = mut,
  file = here('analysis', 'explore', 'mut_test_ready_to_annotate.txt'),
  na = ''
)








