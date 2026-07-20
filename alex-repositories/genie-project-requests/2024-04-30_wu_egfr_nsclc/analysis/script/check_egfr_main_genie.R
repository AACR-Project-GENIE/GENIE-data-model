# Look at the counts of eligible EGFR mutations in main GENIE

dft_clin <- data.table::fread(
  here('data-raw', 'main_genie', 'data_clinical.txt')
) %>%
  rename_all(tolower)

dft_clin %<>% filter(cancer_type %in% "Non-Small Cell Lung Cancer")

dft_maf <- data.table::fread(
  here('data-raw', 'main_genie', 'data_mutations_extended.txt')
)

dft_maf %<>% 
  filter(Tumor_Sample_Barcode %in% dat_clin$sample_id)

dft_maf %<>%
  left_join(
    ., 
    select(dft_clin, Tumor_Sample_Barcode = sample_id, record_id = patient_id),
    by = "Tumor_Sample_Barcode"
  )

n_pt <- dft_maf %>% count(record_id) %>% nrow

cli::cli_inform("The number of PATIENTS in main GENIE with NSCLC is {n_pt}.")


# Big chunk stolen from get_genomic_cohort.R:

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


dft_maf_mut <- dft_maf %>% filter_muts 

n_pt_mut <- dft_maf_mut %>% count(record_id) %>% nrow

cli::cli_inform("The number of PATIENTS in main GENIE with NSCLC and an eligible EGFR mutation is {n_pt_mut} ({round(n_pt_mut/n_pt*100,1)}%).")

