library(here)
library(readr)
library(tidyverse)
library(magrittr)

mut <- readr::read_rds(
    here('data-raw', 'mut.rds')
)

clin <- readr::read_rds(
    here('data-raw', 'clin.rds')
)

mut %<>% rename_all(tolower)
clin %<>% rename_all(tolower)

vec_sponsor_list <- c(
    "R103Q", "I767F", "V777M",
    "D277Y", "I767M", "V777A",
    "G309A", "D769Y", "P780_Y781insGSP/G778_P780dup",
    "G309E", "D769N", "V777_G778insCG",
    "S310P", "D769H", "V777_G778insG",
    "S310F", "A775_G776insYVMA/Y772_A775dup", "V777_G778insV",
    "S310Y", "A775_G776insV", "G778_S779insLPG",
    "E321G", "A775_G776insTVMA", "G778_S779insCPG",
    "R330Q", "G776delinsLC", "G778_S779insLPS",
    "S335C", "G776C", "G778_S779insG",
    "S653C", "G776S", "S779_P780insVGS",
    "V659E", "G776delinsCV", "T798I",
    "G660D", "G776delinsVC", "T798M",
    "R678Q", "G776V", "V842I",
    "T733I", "G776_V777insL", "V842E",
    "L755A", "G776_V777insVGS", "T862I",
    "L755F", "G776_V777insVGC", "R868W",
    "L755M", "G776_V777insVC", "L869R",
    "L755P", "V777delinsCVCG", "R896C",
    "L755S", "V777L", "R896H",
    "L755W"
)

# obtained from the website.  mskcc.oncotree pacakge doesn't seem to have the
#.  right structure, this was easier.
vec_nsclc_oncotree <- c(
    'NSCLC', 'CMPT', 
    'LCLC', 'BLCLC', 'CCLC', 'GCLC', 'RLCLC', 'LECLC',
    'LUAD', 'LUAS', 'LUSC', 'NUTCL', 'LUPC', 'NSCLCPD',
    'SGTTL', 'LUACC', 'LUMEC',
    'SPCC'
)

readr::write_rds(
    vec_nsclc_oncotree,
    here('data', 'included_oncotree_codes.rds')
)

clin_nsclc <- clin %>% 
    filter(
        oncotree_code %in% vec_nsclc_oncotree
    ) %>%
    select(sample_id, oncotree_code, patient_id, seq_year, center)

nsclc_erbb2 <- mut %>%
    filter(hugo_symbol %in% "ERBB2") %>%
    inner_join(
        .,
        select(clin_nsclc, -center),
        by = c(tumor_sample_barcode = "sample_id")
    ) 

vec_sponsor_list <- c(
    "R103Q", "I767F", "V777M",
    "D277Y", "I767M", "V777A",
    # Split these up:
    "G309A", "D769Y", "P780_Y781insGSP",
    "G309E", "D769N", "V777_G778insCG",
    "S310P", "D769H", "V777_G778insG",
    "S310F", "A775_G776insYVMA", "V777_G778insV",
    "S310Y", "A775_G776insV", "G778_S779insLPG",
    "E321G", "A775_G776insTVMA", "G778_S779insCPG",
    "R330Q", "G776delinsLC", "G778_S779insLPS",
    "S335C", "G776C", "G778_S779insG",
    "S653C", "G776S", "S779_P780insVGS",
    "V659E", "G776delinsCV", "T798I",
    "G660D", "G776delinsVC", "T798M",
    "R678Q", "G776V", "V842I",
    "T733I", "G776_V777insL", "V842E",
    "L755A", "G776_V777insVGS", "T862I",
    "L755F", "G776_V777insVGC", "R868W",
    "L755M", "G776_V777insVC", "L869R",
    "L755P", "V777delinsCVCG", "R896C",
    "L755S", "V777L", "R896H",
    "L755W"
) 

# decided to re-sort these, don't mind this mess too much.
vec_sponsor_list <- c(
    vec_sponsor_list[seq.int(1, length(vec_sponsor_list), 3)],
    vec_sponsor_list[seq.int(2, length(vec_sponsor_list), 3)],
    vec_sponsor_list[seq.int(3, length(vec_sponsor_list), 3)]
)

nsclc_erbb2 %<>% mutate(
    hgvsp_short_trim = str_replace(hgvsp_short, "^p.", ""),
    hgvsp_short_trim = case_when(
        # arbitrarily mapping to the first one in their list:
        hgvsp_short_trim %in% "G778_P780dup" ~ "P780_Y781insGSP",
        hgvsp_short_trim %in% "Y772_A775dup" ~ "A775_G776insTVMA",
        T ~ hgvsp_short_trim
    ),
    act_list = hgvsp_short_trim %in% vec_sponsor_list
)

# Add tags to the sample-keyed file for ERBB2 muts.
clin_nsclc <- nsclc_erbb2 %>%
    group_by(tumor_sample_barcode) %>%
    summarize(
        any_erbb2 = T, # auto true based on previous filters.
        act_erbb2 = any(act_list, na.rm = T)
    ) %>%
    rename(sample_id = tumor_sample_barcode) %>%
    left_join(
        clin_nsclc,
        ., 
        by = 'sample_id'
    )

clin_nsclc %<>%
    replace_na(
        list(any_erbb2 = F, act_erbb2 = F)
    )

readr::write_rds(
    clin_nsclc,
    here('data', 'nsclc_main_genie.rds')
)
