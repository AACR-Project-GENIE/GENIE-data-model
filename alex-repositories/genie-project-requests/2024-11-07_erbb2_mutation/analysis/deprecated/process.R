library(here)
library(readr)
library(tidyverse)
library(magrittr)

mut <- readr::read_rds(
    here('data', 'mut.rds')
)

clin <- readr::read_rds(
    here('data', 'clin.rds')
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

# decided to resort these, don't mind this mess too much.
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

# Three numbers in Jumi's slide:
n_nsclc <- clin_nsclc %>% count(patient_id) %>% nrow
n_nsclc_erbb2 <- nsclc_erbb2 %>% count(patient_id) %>% nrow # number with Erbb2
nsclc_erbb2 %>% filter(act_list) %>% count(patient_id) %>% nrow # with erbb2 act mut.

dfp_mut_counts_in_list <- nsclc_erbb2 %>%
    filter(act_list) %>%
    # get one row per patient/hgvsp code - just in case of dupes.
    group_by(hgvsp_short_trim) %>%
    summarize(people = length(unique(patient_id)), .groups = 'drop') %>%
    # count(hgvsp_short_trim, patient_id) %>% 
    # count(hgvsp_short_trim, name = 'people') %>%
    mutate(
        hgvsp_short_trim = factor(hgvsp_short_trim, levels = vec_sponsor_list)
    ) %>%
    complete(hgvsp_short_trim, fill = list(people = 0)) %>%
    mutate(
        prop_nsclc = people / n_nsclc,
        prop_nsclc_erbb2 = people / n_nsclc_erbb2
    ) %>%
    arrange(hgvsp_short_trim)

dfp_all_erbb2_muts <- nsclc_erbb2 %>%
    count(hgvsp_short_trim, act_list, name = "alterations") %>%
    arrange(desc(alterations)) 

dfp_site_nsclc_erbb2 <- nsclc_erbb2 %>%
    filter(act_list) %>%
    # get one row per patient/hgvsp code - just in case of dupes.
    group_by(center) %>%
    summarize(people = length(unique(patient_id)), .groups = 'drop') 

dfp_site_nsclc_erbb2 <- clin_nsclc %>%
    group_by(center) %>%
    summarize(n_nsclc_center = length(unique(patient_id)), .groups = 'drop') %>%
    full_join(dfp_site_nsclc_erbb2, ., by = 'center') %>% 
    replace_na(list(people = 0)) %>%
    mutate(
        prop_nsclc = people / n_nsclc_center,
    ) %>%
    arrange(desc(people))

readr::write_rds(
    dfp_mut_counts_in_list,
    here('data', 'mut_counts_in_list.rds')
)

readr::write_rds(
    dfp_all_erbb2_muts,
    here('data', 'all_erbb2.rds')
)

readr::write_rds(
    dfp_site_nsclc_erbb2,
    here('data', 'site_nsclc_erbb2.rds')
)

readr::write_rds(
    c(n_nsclc, n_nsclc_erbb2, 
      (nsclc_erbb2 %>% filter(act_list) %>% count(patient_id) %>% nrow)),
    here('data', 'three_numbers.rds')
)

# AACR team member asked for the list too:
readr::write_csv(
    (nsclc_erbb2 %>% filter(act_list)),
    here('data', 'nsclc_erbb2_activating_mutations.csv')
)

readr::write_csv(
    (nsclc_erbb2 %>% 
         filter(act_list) %>% 
         select(tumor_sample_barcode, patient_id) %>%
         distinct(.)),
    here('data', 'nsclc_erbb2_cohort.csv')
)

# One more request: PROV samples, what panels did they use?
prov_request <- clin_nsclc %>% 
    filter(center %in% "PROV") %>%
    # put the assay info back in:
    left_join(., select(clin, sample_id, seq_assay_id), by = 'sample_id') %>%
    # add a flag for whether this sample wound up in our cohort:
    mutate(has_act_mut = sample_id %in% (
        nsclc_erbb2 %>% filter(act_list) %>% 
            pull(tumor_sample_barcode) %>% unique))

readr::write_csv(
    prov_request,
    here('data', 'prov_samples.csv')
)

prov_request %>% 
    group_by(seq_assay_id) %>%
    summarize(
        samples = n(),
        samples_with_erbb2_act_mut = sum(has_act_mut),
        prop = samples_with_erbb2_act_mut/samples
    )
