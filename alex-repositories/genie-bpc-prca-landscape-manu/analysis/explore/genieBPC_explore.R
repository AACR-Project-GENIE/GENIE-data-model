library(genieBPC)
library(lobstr)
library(magrittr)

# need to use set_synapse_credentials(), which only supports username and password unfortunately.


nsclc <- pull_data_synapse("NSCLC", version = "v2.0-public")

# also works:
test <- pull_data_synapse("PANC", version = "v1.2-consortium")
# stores the dataframes all in a list (a 35 Mb list)
nsclc$NSCLC_v2.0 %>% class
nsclc$NSCLC_v2.0 %>% names
nsclc$NSCLC_v2.0 %>% lobstr::tree(., max_depth = 1)
nsclc_stg_iv <- create_analytic_cohort(data_synapse = nsclc$NSCLC_v2.0,
                                       stage = c("Stage IV"))
# This is exactly the same structure, so it's a bit odd that they need both for the sunburst helper.
nsclc_stg_iv %>% lobstr::tree(., max_depth = 1)


# potentially helpful:
sunplot <- drug_regimen_sunburst(data_synapse = nsclc$NSCLC_v2.0,
                                 data_cohort = nsclc_stg_iv,
                                 max_n_regimens = 10)
sunplot


# can we remove some of this structure and still make use of the sunburst helper?
# pull out the fusions which we definitely don't need for this:
nsclc_stg_iv <- nsclc_stg_iv[-10]

sunplot <- drug_regimen_sunburst(data_synapse = nsclc$NSCLC_v2.0,
                                 data_cohort = nsclc_stg_iv,
                                 max_n_regimens = 10)
sunplot

# Let's take this a step further:  How much information loss will they tolerate?
# Method was running the below list until with each item present/absent until 
#   a minimal required set was identified.
nsclc_full_test <- list(NSCLC_v2.0 = list())
nsclc_full_test$NSCLC_v2.0 <- nsclc$NSCLC_v2.0[c(1,2,4,8)]
nsclc_stg_iv_test <- nsclc_stg_iv[c(1,2,4,8)]

sunplot <- drug_regimen_sunburst(data_synapse = nsclc_full_test$NSCLC_v2.0,
                                 data_cohort = nsclc_stg_iv_test,
                                 max_n_regimens = 10)
sunplot

# How nice, the required elements are all powers of 2.
# These are:   pt_char, ca_dx_index, ca_drugs, cpt
