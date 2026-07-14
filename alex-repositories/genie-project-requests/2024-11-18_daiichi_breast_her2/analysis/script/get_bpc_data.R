# This uses a criterion for HER2-low passed on by Jen Hoppe.
# HER2 low is people who meet either of the following criteria:
#  1. Have an IHC of 1+ 
#  2. Have an IHC of 2+ AND ISH negative.
# This seems to match the trastuzumab deruxtecan label, but may not be appropriate
#   for other uses.

# We will take the IHC values from the case-level summmaries in the cancer
#   index dataset.  For the ISH values we summarize each person in the pathology
#   records, marking anyone who has at least one negative ISH test.  A more
#   strict criterion would be requiring that there are no positive ISH tests.
#   To use the strict criterion, just toggle the following flag to TRUE:
use_strict_filtering <- TRUE 

library(tidyverse)
library(readr)
library(here)

synLogin()

# Update data paths as needed for your system.
dft_ca_ind <- get_synapse_entity_txt(
    synapse_id = 'syn43172806', sep = ","
)
dft_path <- get_synapse_entity_txt(
    synapse_id = 'syn43172879', sep = ","
)

dft_ca_ind %<>% as_tibble
dft_path %<>% as_tibble

dft_her2_low <- dft_ca_ind %>%
    select(record_id, ca_seq, ca_bca_her2ihc_val)

dft_ish_summary <- dft_path %>% 
    select(record_id, path_proc_number, path_rep_number,
           matches("^path_her[2]*ish_[1-5]")) %>%
    pivot_longer(
        cols = matches("^path_her[2]*ish_[1-5]")
    )

neg_ihc_vec <- c("Negative")
if (use_strict_filtering) {
    dft_ish_summary <- dft_ish_summary %>%
        group_by(record_id) %>%
        summarize(ish_neg = any(value %in% neg_ihc_vec) & all(!value %in% "Positive"))
} else {
    dft_ish_summary <- dft_ish_summary %>%
        group_by(record_id) %>%
        summarize(ish_neg = any(value %in% neg_ihc_vec))
}

dft_her2_low <- left_join(
    dft_her2_low, 
    dft_ish_summary,
    by = c("record_id")
)

dft_her2_low <- dft_her2_low %>%
    mutate(
        ihc_1 = ca_bca_her2ihc_val %in% "Score of 1+",
        ihc_2_ish_neg = ca_bca_her2ihc_val %in% "Score of 2+" & ish_neg,
        her2_low = ihc_1 | ihc_2_ish_neg
    )

# Adding POSSIBLE HER2 ultra-low - which is a subcategory of IHC = 0 and HER2-.
dft_her2_low <- dft_her2_low %>%
    mutate(
        her2_ultra_low_possible = ca_bca_her2ihc_val %in% "Score 0" & ish_neg
    )

# Counts of CASES (more than one per person possible):
dft_her2_low %>% summarize(across(ihc_1:her2_low, .fns = sum))

# Counts of PEOPLE (ignoring case order)
dft_her2_low %>% 
    arrange(desc(her2_low)) %>%
    group_by(record_id) %>%
    slice(1) %>% # one row per person
    ungroup %>%
    summarize(across(ihc_1:her2_low, .fns = sum))

# Counts of PEOPLE who only had ONE cancer (a breast cancer):
dft_her2_low %>% 
    filter(ca_seq %in% 0) %>%
    summarize(across(ihc_1:her2_low, .fns = sum))






# Now doing the dmet time first:
n_breast <- count(dft_ca_ind, record_id) %>% nrow # number of people in this cohort.
n_breast_met <- get_dmet_timing(dft_ca_ind) %>% nrow # number of people with met dx anytime
n_breast_met_ish <- dft_her2_low %>%
    group_by(record_id) %>%
    arrange(desc(ish_neg)) %>%
    slice(1) %>%
    select(record_id, ish_neg) %>%
    left_join(
        get_dmet_timing(dft_ca_ind),
        ., 
        by = 'record_id'
    ) %>%
    filter(ish_neg) %>%
    nrow

n_breast_met_ish_ihc_zero <- dft_her2_low %>%
    # take their her2 low cancer if they have more than one:
    group_by(record_id) %>%
    arrange(desc(her2_ultra_low_possible)) %>%
    slice(1) %>%
    select(record_id, her2_ultra_low_possible) %>%
    left_join(
        get_dmet_timing(dft_ca_ind),
        ., 
        by = 'record_id'
    ) %>%
    filter(her2_ultra_low_possible) %>%
    nrow

attrition_table <- tribble(
    ~step, ~n_bpc,
    'Breast cancer cases', n_breast,
    'Metastatic dx (anytime)', n_breast_met,
    'ISH neg', n_breast_met_ish,
    'IHC = 0', n_breast_met_ish_ihc_zero
)

readr::write_rds(
    attrition_table,
    here('data', 'bpc_attrition.rds')
)

