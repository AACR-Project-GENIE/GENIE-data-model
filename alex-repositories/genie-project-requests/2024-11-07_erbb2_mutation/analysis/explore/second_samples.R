library(here)
library(tidyverse)
library(magrittr)
library(glue)

clin <- readr::read_rds(
    here('data-raw', 'clin.rds')
)

# oncotree codes from the data guide and a box file shawn had (https://app.box.com/file/1722721259582)

nsclc_box <- c('CMPT', 'SPCC', 'SGTTL', 'NUTCL', 'LCLC', 'LUPC', 'LUSC',
               'NSCLCPD', 'LUMEC', 'LECLC', 'CCLC', 'RLCLC', 'GCLC', 
               'LUACC', 'BLCLC', 'NSCLC')
nsclc_dg <- c('NSCLC', 'CMPT', 'LCLC', 'LUAD', 'LUSC', 'LUPC', 'NSCLCPD',
              'LUAS', 'LUMEC', 'LECLC', 'CCLC', 'RLCLC', 'GCLC', 'BLCLC')

bladder_box <- c('BLCA', 'UTUC', 'BLAD', 'SCBC', 'BLSC')
bladder_dg <- bladder_box

breast_box <- c('BRCA', 'BRCANOS', 'BRCNOS', 'IDC', 'ILC', 'MDLC')
breast_dg <- c('BA', 'BRCA', 'BRCANOS', 'BRCNOS', 'IDC', 'ILC', 'MBC',
               'MDLC', 'MMBC', 'MPT', 'MSCC')

pancreatic_box <- c('PAAD', 'PAASC', 'PAAC', 'UCP')
pancreatic_dg <- c('PAAD', 'PAASC', 'PAAC', 'UCP')

prostate_box <- c('PROSTATE', 'BCCP', 'PRSC', 'PRAD', 'PRSCC', 'PRNE')
prostate_dg <- c('PRAD') # yikes.

crc_box <- c('COAD', 'READ', 'COADREAD', 'MACR', 'SRCCR')
crc_dg <- c('COADREAD', 'COAD', 'MACR', 'SRCCR', 'READ')

esophagogastric_box <- c('ESCA', 'STAD', 'GEJ', 'EGC', 'SPDAC', 'SSRCC',
                       'ESCC', 'EPDCA', 'DSTAD', 'ISTAD', 'TSTAD', 'MSTAD',
                       'USTAD', 'PSTAD', 'GRC')
melanoma_box <- c('SKCM', 'MEL')
renal_box <- c('CCRCC', 'RCC', 'PRCC', 'NCCRCC', 'SCCRCC', 'CCPRC')
ovarian_box <- c('HGSOC', 'SOC', 'CCOV', 'EOV', 'HGSFT', 'OVT', 'LGSOC')

oncotree_key <- bind_rows(
    tibble(cancer_type = 'NSCLC', oncotree_code = nsclc_box),
    tibble(cancer_type = 'NSCLC', oncotree_code = nsclc_dg),
    tibble(cancer_type = 'Bladder', oncotree_code = bladder_box),
    tibble(cancer_type = 'Bladder', oncotree_code = bladder_dg),
    tibble(cancer_type = 'Breast', oncotree_code = breast_box),
    tibble(cancer_type = 'Breast', oncotree_code = breast_dg),
    tibble(cancer_type = 'Pancreatic', oncotree_code = pancreatic_box),
    tibble(cancer_type = 'Pancreatic', oncotree_code = pancreatic_dg),
    tibble(cancer_type = 'Prostate', oncotree_code = prostate_box),
    tibble(cancer_type = 'Prostate', oncotree_code = prostate_dg),
    tibble(cancer_type = 'CRC', oncotree_code = crc_box),
    tibble(cancer_type = 'CRC', oncotree_code = crc_dg),
    tibble(cancer_type = 'Esophagogastric', oncotree_code = esophagogastric_box),
    tibble(cancer_type = 'Melanoma', oncotree_code = melanoma_box),
    tibble(cancer_type = 'Renal', oncotree_code = renal_box),
    tibble(cancer_type = 'Ovarian', oncotree_code = ovarian_box)
)

oncotree_key %<>%
    distinct(.) %>%
    rename(cancer_type_custom = cancer_type)

oncotree_key$oncotree_code %>% duplicated(.) %>% any(.)

clin %<>%
    rename_all(tolower) %>% 
    mutate(
        seq_date = lubridate::my(seq_date),
        age_at_seq_report_days = case_when(
            # fine for this:
            age_at_seq_report_days %in% "<6570" ~ 6569,
            age_at_seq_report_days %in% ">32485" ~ 32486,
            age_at_seq_report_days %in% "Unknown" ~ NA_real_,
            T ~ as.numeric(age_at_seq_report_days)
        )
    ) %>%
    left_join(
        .,
        oncotree_key, 
        by = "oncotree_code",
        relationship = 'many-to-one'
    )

clin %<>% 
    replace_na(list(cancer_type_custom = "Other")) %>%
    select(sample_id, patient_id, seq_year, age_at_seq_report_days, cancer_type_custom)

clin %<>%
    group_by(patient_id) %>%
    # ties could still be broken by seq_year for <18 or >89 year old patients.
    arrange(age_at_seq_report_days, seq_year, cancer_type_custom %in% "Other") %>%
    mutate(sample_number = 1:n(),
           cancer_type_custom_first_samp = first(cancer_type_custom)) %>%
    ungroup(.)



sum_cancer_type <- clin %>%
    filter(sample_number %in% 2) %>%
    group_by(seq_year, cancer_type_custom_first_samp) %>%
    summarize(
        second_sample_events = length(unique(patient_id)),
        check = length(unique(sample_id)),
        .groups = 'drop'
    )

if (any(sum_cancer_type$second_sample_events != sum_cancer_type$check)) {
    cli_abort("Something wrong structurally, check")
} else {
    sum_cancer_type <- select(sum_cancer_type, -check)
}

sum_cancer_type %<>% filter(!is.na(seq_year)) # nothing we can use here.

sum_cancer_type %<>%
    arrange(cancer_type_custom_first_samp, seq_year) %>%
    group_by(cancer_type_custom_first_samp) %>%
    mutate(cum_sec_samp = cumsum(second_sample_events)) %>%
    ungroup(.)

# Double samples in the last year:
sum_current <- sum_cancer_type %>%
    group_by(cancer_type_custom_first_samp) %>%
    slice(n()) %>%
    select(cancer_type_custom_first_samp, cum_sec_samp, seq_year) %>%
    ungroup(.) %>% 
    arrange(cancer_type_custom_first_samp %in% 'Other')

get_slope <- function(x,y) {
    lm(y ~ x) %>%
        broom::tidy(.) %>% 
        filter(term %in% 'x') %>%
        pull(estimate)
}


sum_slope <- sum_cancer_type %>%
    group_by(cancer_type_custom_first_samp) %>%
    filter(seq_year >= 2022) %>%
    summarize(
        slope = get_slope(seq_year, cum_sec_samp),
        .groups = 'drop'
    ) %>% 
    arrange(cancer_type_custom_first_samp %in% 'Other')
    
output_df <- full_join(sum_current, sum_slope, by = 'cancer_type_custom_first_samp')

output_df %>%
    mutate(
        str = glue('{cancer_type_custom_first_samp}: {cum_sec_samp} total patients with >= 2 samples in 2023, growing at a rate of {round(slope)} people per year.')
    ) %>%
    pull(str) %>%
    paste(., collapse = '\n') %>%
    cat
    






