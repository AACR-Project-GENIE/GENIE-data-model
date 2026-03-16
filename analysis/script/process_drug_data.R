library(cli) # prevents an error on version reliance.
library(readr)
library(vctrs)
library(rlang)
library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(janitor)
library(glue)
library(genieBPC)
library(ggplot2)
library(sunburstR)
library(huxtable)

purrr::walk(.x = here("R", dir(here("R"))), .f = source)


data_list <- readr::read_rds(here("data-raw", "data_list.rds"))


# Update May 18: Remove participants with Sarcomas.  
#   The paper is limited in scope to only those without sarcoma, and
#   this matches with Protiva's previous work.
vec_not_sarcoma <- data_list$BrCa_v1.2$ca_dx_index %>%
  select(record_id, ca_seq, ca_hist_adeno_squamous) %>%
  filter(!(ca_hist_adeno_squamous %in% "Sarcoma")) %>%
  pull(record_id)

data_list$BrCa_v1.2 <- data_list$BrCa_v1.2 %>% 
  purrr::map(
    .x = .,
    .f = (function(x) filter_df_by_rid(x, vec_not_sarcoma))
  )






# dft_med_classified <- readr::read_csv(
#   file = here("data", "tx_by_class_metastatic_filt_cohort.csv")
# )


# Note: The file I had loaded before did not have the same column names.
#  I loaded a different file that Brooke created to try and fix this, which
#  appeared to have the correct columns.
dft_med_classified <- readr::read_csv(
  file = here('data', 'tx_by_class_metastatic_filt_cohort.csv')
)

dft_med_classified <- dft_med_classified %>%
  select(
    record_id = PATIENT_ID,
    regimen_number = REGIMEN_NUMBER,
    ca_seq = CA_SEQ, 
    agent = AGENT, 
    regimen = REGIMEN,
    class1 = Class1,
    class1.1 = Class1.1,
    exclude = Exclude
  )





# Update from May 10: We want a compromise between class1 and class1.1.
# Pedram mentioned at least 3 categories being pulled out:
# - anti-her2, which is targetted by class1.
# - pd3k, which is targetted by class1.
# - possibly pdl1?  Can't recall on that, skipping for now.

vec_class1.1 <- c("antiher2", 
                  "pi3k pathway inhibitor",
                  "antivegf",
                  "parpi")

dft_med_classified %<>%
  # Causes problems with sunburst plots where "-" separates rings.
  # Update: we're done with sunburst plots I think so time to fix:
  # mutate(class1.1 = stringr::str_replace(class1.1,
  #                                        "-",
  #                                        "")) %>%
  mutate(class_comp = case_when(
    is.na(class1) ~ NA_character_,
    class1 %in% "targeted" & 
      class1.1 %in% vec_class1.1 ~ class1.1, 
    class1 %in% "targeted" ~ "other targeted",
    T ~ class1
  ))

dft_drug_map <- dft_med_classified %>%
  select(agent, class1, class1.1, exclude, class_comp) %>%
  group_by(agent) %>% 
  slice(1) %>%
  ungroup()

# Update May 17: Shawn made some additional recommendations here:
dft_drug_map %<>%
  mutate(exclude = if_else(agent %in% "Rituximab", 1, exclude)) %>%
  mutate(
    class_comp = case_when(
      agent %in% c(
        "Ipilimumab", 
        "Nivolumab",
        "Pembrolizumab",
        "Atezolizumab",
        "Avelumab"
      ) ~ "IC inhibitor", # immune checkpoint inhibitor.
      agent %in% c(
        "Trastuzumab Deruxtecan",
        "Trastuzumab Emtansine",
        "Lapatinib Ditosylate",
        "Neratinib",
        "Tucatinib"
      ) ~ "antiher2",
      agent %in% c(
        "Abemaciclib",
        "Palbociclib",
        "Ribociclib"
      ) ~ "CDK inhibitor",
      agent %in% c(
        "Apatinib",
        "Cabozantinib Smalate",
        "Lenvatinib Mesylate",
        "Pazopanib Hydrochloride",
        "Sorafenib Tosylate"
      ) ~ "antivegf",
      agent %in% c(
        "Trametinib"
      ) ~ "pi3k pathway inhibitor",
      # This one is excluded, but just in case:
      agent %in% c(
        "Tegafurgimeraciloteracil Potassium"
      ) ~ "chemo",
      agent %in% "Rituximab" ~ NA_character_,
      T ~ class_comp
    )
  )

# Output drug map - originally for feedback from Shawn,
#   now just for general practice.
dft_drug_map %>%
  arrange(class1, class1.1, agent) %>%
  readr::write_csv(x = .,
                   file = here("data", "drug_map.csv"))

dft_drug_map %<>%
  arrange(agent) %>%
  select(agent, class_comp)
  
dft_reg_map <- dft_med_classified %>%
  group_by(record_id, regimen_number, ca_seq, regimen) %>%
  summarize(
    regimen_cc = paste0(sort(unique(class_comp)), collapse = ", "),
    .groups = "drop"
  ) %>%
  group_by(regimen) %>% 
  slice(1) %>%
  select(regimen, regimen_cc) %>%
  ungroup()



# Only accept
# Confirmed on May 15 this is what Brooke did too.
data_list$BrCa_v1.2$ca_dx_index <- data_list$BrCa_v1.2$ca_dx_index %>%
  group_by(record_id) %>% 
  slice(1) %>%
  ungroup













data_list_cc <- data_list

# replace the list of drugs with the list of regimens from this set:
data_list_cc$BrCa_v1.2$ca_drugs <- data_list_cc$BrCa_v1.2$ca_drugs %>%
  mutate(regimen_drugs = as.character(regimen_drugs)) %>%
  left_join(
    .,
    select(dft_reg_map, regimen, regimen_cc),
    by = c(regimen_drugs = "regimen")
  ) %>%
  filter(!(regimen_cc %in% "") & !is.na(regimen_cc)) %>%
  mutate(regimen_drugs = factor(regimen_cc)) %>%
  select(-regimen_cc)


data_list_cc$BrCa_v1.2$ca_drugs <- 
  data_list_cc$BrCa_v1.2$ca_drugs %>% 
  mutate(
    across(
      .cols = matches("^drugs_drug_[0-9]$"),
      .fns = (function(x) {
        x <- as.character(x)
        x <- stringr::str_replace(x, "\\(.*", "")
        x <- map_drug(x, dft_drug_map, type = "class_comp")
        return(x)
      })
    )) 









dft_dmet_timing <- get_dmet_timing(
  ca_ind_df = data_list_cc$BrCa_v1.2$ca_dx_index
)

prog_cohort_cc <- filter_dl_by_pt(
  d_list = data_list_cc,
  dft_dmet_timing
)

save(
  data_list,
  data_list_cc,
  dft_dmet_timing,
  dft_med_classified,
  dft_drug_map,
  dft_reg_map,
  prog_cohort_cc,
  file = here("data", "prog_cohorts.Rda")
)



