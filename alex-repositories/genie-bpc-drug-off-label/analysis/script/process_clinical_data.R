# Description:  Transform data as needed and save in /data/cohort.

library(fs); library(purrr); library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

dft_data_names <- tribble(
  ~short_name, ~synapse_name,
  "ca_ind", "cancer_level_dataset_index.csv",
  "ca_non_ind", "cancer_level_dataset_non_index.csv",
  "cpt", "cancer_panel_test_level_dataset.csv",
  "pt", "patient_level_dataset.csv",
  "reg", "regimen_cancer_level_dataset.csv"
)


dft_clin_dat <- tibble(
  cohort = dir("data-raw")
) %>%
  mutate(cohort_path = here("data-raw", cohort)) %>%
  filter(cohort %in% c("BLADDER", 'BrCa', 'CRC', 'NSCLC', 'PANC', 'Prostate'))

# A bit inefficient here - we load all the data, then trim down to the ones 
#  we want to keep. 
dft_clin_dat %<>%
  mutate(
    loaded_data = purrr::map(
      .x = cohort_path,
      .f = (function(p) {
        folder_load_helper(p, dn = dft_data_names)
      })
    )
  ) %>%
  unnest(loaded_data)

# change the cohort name from "NSCLC2" to "NSCLC".  We have the "phase" column
# if we ever want to recover that info, not sure why they did this.
dft_clin_dat %<>%
  mutate(
    dat = purrr::map(
      .x = dat,
      .f = \(z) {
        mutate(z, cohort = if_else(cohort %in% "NSCLC2", "NSCLC", cohort))
      }
    )
  )

readr::write_rds(
  dft_clin_dat,
  here('data-raw', 'clin_dat_untouched.rds')
)

# Special concern:  Remove breast sarcomas.
vec_breast_sarcoma <- dft_clin_dat %>% 
  filter(short_name %in% "ca_ind" & cohort %in% "BrCa") %>%
  pull(dat) %>%
  `[[`(.,1) %>%
  filter(ca_hist_adeno_squamous %in% "Sarcoma") %>%
  pull(record_id)
# Note:  We can ignore ca_seq here because we happen to be limiting to 
# first and only cancers later on.  Anyone with a first sarcoma will be eliminated, and everyone with a ca_seq >= 1 will be eliminated 
# later on no matter what we do.


dft_clin_dat %<>%
  mutate(
    dat = map2(
      .x = dat,
      .y = cohort,
      .f = \(d,coh) {
        if (coh %in% "BrCa") {
          d %>% filter(!(record_id %in% vec_breast_sarcoma))
        } else {
          d # do nothing.
        }
      }
    )
  )
          



dft_clin_dat_wide <- dft_clin_dat %>%
  select(cohort, short_name, dat) %>%
  pivot_wider(
    names_from = short_name,
    values_from = dat
  )

# Track the flow of drugs as we subset and limit things:
dft_drug_tracking <- tibble(
  step = "Raw data",
  drug_key = list(tracking_hdrug_helper(dft_clin_dat_wide,
                                        dat_name = "reg"))
)






# Fix the breast-type deviances in regimen data:
dft_clin_dat_wide %<>%
  mutate(
    # hreg = harmonized regimen data.
    hreg = purrr::map(
      .x = reg,
      .f = \(x) {
        breast_regimen_fix(x) 
      }
    )
  )

# Later on we will need a range of possible values for birth date.  Just add now.
dft_clin_dat_wide %<>%
  mutate(
    pt = purrr::map(
      .x = pt,
      .f = add_birth_date_range
    )
  )

readr::write_rds(
  dft_clin_dat_wide,
  here('data', 'no_ca_seq_filter', 'clin_dat_just_before_drug_removal.rds')
)

# Remove drugs with a non-standard administration route (decided Nov 21, 2023)
dft_clin_dat_wide %<>%
  mutate(
    hreg = purrr::map(
      .x = hreg,
      .f = remove_nonstandard_admin_drugs
    )
  )

dft_drug_tracking <- bind_rows(
  dft_drug_tracking, 
  tibble(
    step = "Nonstandard admin routes removed",
    drug_key = list(tracking_hdrug_helper(dft_clin_dat_wide))
  )
)

dft_clin_dat_wide %<>%
  mutate(
    # hreg = harmonized regimen data.
    hreg = purrr::map(
      .x = hreg,
      .f = select_hreg_columns
    )
  )


# Add the versions of the regimen variables that include last known administration.
dft_clin_dat_wide %<>%
  mutate(
    hreg = purrr::map(
      .x = hreg,
      .f = \(x) {
        add_regimen_lastadm_vars(x)
      }
    )
  )

# Remove any investigational drug regimens from the dataset.
dft_clin_dat_wide %<>%
  mutate(
    hreg = purrr::map(
      .x = hreg,
      .f = \(x) {
        x %>%
          filter(
            str_detect(
              regimen_drugs, 
              "Investigational Drug", 
              negate = T
            )
          )
      }
    )
  )


dft_clin_dat_wide %<>%
  mutate(
    hreg = purrr::map(
      .x = hreg,
      .f = remove_clinical_trial_regimens
    )
  )

dft_drug_tracking <- bind_rows(
  dft_drug_tracking, 
  tibble(
    step = "Remove clinical trial and investigational regimens",
    drug_key = list(tracking_hdrug_helper(dft_clin_dat_wide))
  )
)



# Create a drug-keyed dataset:
dft_clin_dat_wide %<>%
  mutate(
    hdrug = purrr::map(
      .x = hreg,
      .f = create_drug_dat
    )
  )

# Trim whitespace from agent names.  Now a bigger problem in NSCLCv3.1
dft_clin_dat_wide %<>%
  mutate(
    hdrug = purrr::map(
      .x = hdrug,
      .f = \(z) {
        mutate(z, agent = str_trim(agent))
      }
    )
  )

# Change "HCL" to Hydrochloride now that we have round 2 of NSCLC... just why?
dft_clin_dat_wide %<>%
  mutate(
    hdrug = purrr::map(
      .x = hdrug,
      .f = \(z) {
        mutate(z, agent = str_replace(agent, "HCL", "Hydrochloride"))
      }
    )
  )

# Remove several drugs entirely from the drugs sheet:
ignored_drugs <- c(
  "BCG Solution",
  "BCG Vaccine",
  "Floxuridine",
  "ADT/LHRH agonist not specified",
  "Other antineoplastic",
  "Other NOS",
  "Other hormone",
  # Feb 2024 decision:  
  # We decided not to use interferon because it's too oddly encoded to meaningfully check.
  "Interferon",
  "Recombinant Interferon Alfa",
  "Interferon Alfacon1",
  "Peginterferon Alfa2b"
)

dft_clin_dat_wide %<>%
  mutate(
    hdrug = purrr::map(
      .x = hdrug,
      .f = \(x) {
        x %>%
          filter(!agent %in% ignored_drugs)
      }
    )
  )

dft_drug_tracking <- bind_rows(
  dft_drug_tracking, 
  tibble(
    step = "Remove ignored drugs (interferon, others, BCG, etc.)",
    drug_key = list(tracking_hdrug_subset(dft_clin_dat_wide))
  )
)


# Fix one drug names which cause particular problems down the line.
# "Etoposide" only has phosphate appended once and it causes some 
#    headaches on linking to the Warner materials to have both.  Easier to
#    change the data in this one case.
dft_clin_dat_wide %<>%
  mutate(
    hdrug = purrr::map(
      .x = hdrug,
      .f = \(x) {
        x %>%
          mutate(
            agent = if_else(
              agent %in% "Etoposide Phosphate",
              "Etoposide",
              agent
            )
          )
      }
    )
  )



# A few of the cohort names just annoy me:
dft_clin_dat_wide %<>% fix_cohort_names




readr::write_rds(
  file = here('data', 'no_ca_seq_filter', 'clin_dat_wide.rds'),
  x = dft_clin_dat_wide
)









# New proposal: we will start with "first and only" cancer cases.
# This is neatly described in PRISSMM fortunately.
# Later on this will probably get significantly more annoying.
help_first_and_only <- function(dat) {
  filter(dat, ca_seq %in% 0) 
}

help_us_sites <- function(dat) {
  dat %>%
    mutate(record_sub = str_sub(record_id, 1, 9)) %>%
    filter(!(record_sub %in% "GENIE-UHN")) %>%
    select(-record_sub)
}

dft_clin_dat_wide %<>%
  mutate(
    across(
      .cols = -c(cohort, pt),
      .fns = (function(l_of_dat) {
        purrr::map(
          .x = l_of_dat,
          .f = \(x) {
            x %>%
              help_us_sites(.)
          }
        )
      })
    )
  )


dft_drug_tracking <- bind_rows(
  dft_drug_tracking, 
  tibble(
    step = "Remove non-US sites (UHN)",
    drug_key = list(tracking_hdrug_subset(dft_clin_dat_wide))
  )
)


dft_clin_dat_wide %<>%
  mutate(
    across(
      .cols = -c(cohort, pt),
      .fns = (function(l_of_dat) {
        purrr::map(
          .x = l_of_dat,
          .f = \(x) {
            x %>%
              help_first_and_only(.)
          }
        )
      })
    )
  )

# You will now notice that the ca_non_ind column has no data, as expected:
# lapply(dft_clin_dat_wide$ca_non_ind, nrow)
# We could delete it for coherence, but I'm relying on having it so... nevermind.

dft_drug_tracking <- bind_rows(
  dft_drug_tracking, 
  tibble(
    step = "Limit to first-and-only cancers",
    drug_key = list(tracking_hdrug_subset(dft_clin_dat_wide))
  )
)


dft_clin_dat_wide %<>%
  # To do the first-and-only thing patient dataset we'll just limit to those 
  #   which are in the ca_ind column:
  mutate(
    pt = purrr::map2(
      .x = pt,
      .y = ca_ind,
      .f = \(x,y) {
        r <- y$record_id
        x <- filter(x, record_id %in% r)
        return(x)
      }
    )
  )






# Add information for whether the participant was metastatic at the time of
#   starting each agent.
dft_clin_dat_wide %<>%
  mutate(
    hdrug = purrr::map2(
      .x = hdrug,
      .y = ca_ind,
      # Function: Merge the met time in, check if it's after drug start or not.
      .f = \(dat_drug, dat_ca_ind) {
        met_df <- get_dmet_time(dat_ca_ind) 
        
        rtn <- left_join(
          dat_drug,
          met_df,
          by = c('record_id', 'ca_seq'),
          relationship = "many-to-one"
        )
        
        rtn %<>%
          mutate(
            dmet_at_drug_start = case_when(
              is.na(dx_dmet_yrs) ~ F,
              is.na(dx_drug_start_int) ~ F, # never happens
              dx_drug_start_int < dx_dmet_yrs ~ F,
              T ~ T
            )
          )
          
        return(rtn)
      }
    )
  )


# Add variables for the range of days the drug use could have been on.
dft_clin_dat_wide %<>%
  mutate(
    hdrug = purrr::map2(
      .x = pt,
      .y = hdrug,
      # Function: Merge the met time in, check if it's after drug start or not.
      .f = add_drug_start_range
    )
  )




###############
# Output data #
###############

# I'm switching output formats to just one object:
readr::write_rds(
  x = dft_clin_dat_wide,
  file = here('data', 'cohort', 'clin_dat_wide.rds')
)
readr::write_rds(
  x = dft_drug_tracking,
  file = here('data', 'cohort', 'drug_tracking_01.rds')
)

