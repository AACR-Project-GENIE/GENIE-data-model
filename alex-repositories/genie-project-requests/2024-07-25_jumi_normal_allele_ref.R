# Request from Jumi to look into sites that have tumor only sequencing but
#   values filled in the matched normal column (makes no sense).

library(magrittr)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(cfmisc) # just used for n_pct_str() - on Alex's github page.

samp <- readr::read_tsv(
    paste0(GENIE.dir,"data_clinical_sample.txt"),
    skip = 4
)

pt <- readr::read_tsv(
    paste0(GENIE.dir,"data_clinical_patient.txt"),
    skip = 0,
    comment = "#"
)

ai <- readr::read_tsv(
    paste0(GENIE.dir,"assay_information.txt"),
    skip = 0
)

mut <- data.table::fread(
    paste0(GENIE.dir, "data_mutations_extended.txt")
)

samp_calling <- samp %>%
    left_join(
        .,
        select(pt, PATIENT_ID, CENTER),
        by = "PATIENT_ID"
    ) %>%
    select(SAMPLE_ID, PATIENT_ID, CENTER, SEQ_ASSAY_ID) %>%
    left_join(
        .,
        select(ai, SEQ_ASSAY_ID, calling_strategy),
        by = "SEQ_ASSAY_ID"
    ) 
        
mut %<>%
    left_join(
        .,
        samp_calling,
        by = c(Tumor_Sample_Barcode = "SAMPLE_ID")
    )

mut %>% 
    count(Match_Norm_Seq_Allele1)

# Just want to see if they have different numbers:
mut %>% 
    select(matches("^Match")) %>%
    # proportion of rows with non-missing values:
    purrr::map(.x = ., .f = \(z) mean(!is.na(z) & !(z %in% "")))
# So barcode is filled out about 6x as frequently as the allele.
# They're never NA, blank strings are used - so that was redundant.

# Is one allele ever missing without the other?
mut %>%
    filter(
        xor(
            (!Match_Norm_Seq_Allele1 %in% ""),
            (!Match_Norm_Seq_Allele2 %in% "")
        )
    ) %>%
    select(matches("^Match")) %>%
    as_tibble(.) %>%
    sample_n(20)
# Looks like yes, so let's create a derived variable.

mut %<>%
    mutate(
        Match_Norm_Seq_Allele1or2_not_blank = (
            (!Match_Norm_Seq_Allele1 %in% "") |
            (!Match_Norm_Seq_Allele2 %in% "")
        ),
        Matched_Norm_Sample_Barcode_not_blank = 
           (!Matched_Norm_Sample_Barcode %in% "")
    )

mut_sum <- mut %>%
    group_by(CENTER, SEQ_ASSAY_ID, calling_strategy) %>%
    summarize(
        n_row = n(),
        n_barcode_filled = sum(Matched_Norm_Sample_Barcode_not_blank),
        n_allele1or2_filled = sum(Match_Norm_Seq_Allele1or2_not_blank)
    )

mut_sum %>%
    group_by(CENTER, calling_strategy) %>%
    summarize(
        across(
            .cols = matches("^n_"),
            .fns = sum
        ),
        .groups = "drop"
    ) %>%
    mutate(
        prop_barcode = n_barcode_filled / n_row,
        prop_allele = n_allele1or2_filled / n_row,
        str_barcode = n_pct_str(n_barcode_filled, n_row),
        str_allele = n_pct_str(n_allele1or2_filled, n_row)
    ) %>%
    arrange(calling_strategy, desc(prop_allele), desc(prop_barcode), CENTER) %>%
    select(CENTER, calling_strategy, matches("^str")) %>%
    print(n = 500)

# One thing I don't get is the high proportion of sites that have barcodes 
#   with tumor_only data - what's up with those?

mut %>%
    filter(calling_strategy %in% "tumor_only") %>%
    filter(CENTER %in% c("CHOP", "COLU", "MDA", "NKI", "SCI", 'UCSF', 'YALE',
                         'UHN')) %>%
    mutate(barcode_just_says_normal = Matched_Norm_Sample_Barcode %in% "NORMAL") %>%
    count(CENTER, barcode_just_says_normal) %>%
    as_tibble(.) %>%
    print(n = 500)

# What about the tumor-normal sites?  Do they do this?
mut %>%
    filter(calling_strategy %in% "tumor_normal") %>%
    mutate(barcode_just_says_normal = Matched_Norm_Sample_Barcode %in% "NORMAL") %>%
    count(CENTER, barcode_just_says_normal) %>%
    as_tibble(.) %>%
    print(n = 500)
        
mut %>%
    filter(calling_strategy %in% "tumor_normal") %>%
    count(CENTER)
    
