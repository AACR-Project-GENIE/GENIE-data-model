
library(purrr); library(fs); library(here)
purrr::walk(.x = fs::dir_ls('R'), .f = source)

dft_maf <- readr::read_tsv(
  here('data-raw', 'genomic', 'data_mutations_extended.txt')
)
dft_cpt <- readr::read_csv(
  here('data-raw', 'cancer_panel_test_level_dataset.csv')
)


# Define the mutation filtering:
mut_prot_symbols <- c("L858R")

mut_regex <- paste(paste0("^p.", mut_prot_symbols, "$"), collapse = "|")
# more complicated function copied over from another project:
filter_muts <- function(dat) {
  dat %>%
    mutate(
      alt_desc = case_when(
        Exon_Number %in% "19/28" & Consequence %in% "inframe_deletion" ~ "Exon 19 inframe del",
        # str_detect(HGVSp_short, "p.G719") ~ "p.G719Xaa",
        str_detect(HGVSp_Short, mut_regex) ~ HGVSp_Short,
        T ~ NA_character_
      )
    ) %>%
    filter(!is.na(alt_desc))
}

dft_maf %>% filter(Exon_Number %in% "19/28") %>% count(Consequence)



dft_maf_mut <- dft_maf %>% filter_muts 

dft_maf_mut %<>%
  select(
    cpt_genie_sample_id = Tumor_Sample_Barcode,
    alt_desc
  )

dft_maf_mut %<>%
  group_by(cpt_genie_sample_id) %>%
  summarize(
    alterations = paste(sort(unique(alt_desc)), collapse = ", ")
  )

dft_maf_mut <- dft_cpt %>% 
  select(cpt_genie_sample_id, record_id) %>%
  left_join(dft_maf_mut, ., by = "cpt_genie_sample_id")

readr::write_rds(
  dft_maf_mut,
  here('data', 'samples_with_mut.rds')
)
  
  
    
  