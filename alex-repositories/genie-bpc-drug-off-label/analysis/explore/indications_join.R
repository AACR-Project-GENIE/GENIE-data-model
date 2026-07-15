library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_ind <- readr::read_csv(
  here('data-raw', 'manual', 'indications working copy.csv')
)

dft_cohort_cases <- readr::read_rds(
  here('data', 'cohort', 'clin_dat_wide.rds')
)

dft_hdrug_cohort <- dft_cohort_cases %>%
  select(cohort, hdrug) %>%
  unnest(hdrug)

dft_ind %>% count(condition, sort = T)
dft_ind %>% count(component, sort = T)

dft_hdrug_cohort <- dft_cohort_cases %>%
  select(cohort, hdrug) %>%
  unnest(hdrug)

dft_ind %>% 
  count(condition, sort = T) %>%
  print(n = 500)
dft_hdrug_cohort %>% pull(cohort) %>% unique


# Notes on this:
# - I did not include pancreatic NET as neuroendocrine tumors of the pancreas
#.   didn't appear in the oncotree codes for the data guide.
# - Malignant solid neoplasm NTRK-mutated could have things later on.
# - Skeletal metastases from solid tumors could have things later on.
# - Small bowel adenocarinoma was not added (considered for CRC) as the SBC onoctree code was not part of the inclusion criteria.
# - Benign prostatic hyperplasia was not included for prostate.
# - Several strings with "malignant neoplasm" are listed.
dft_cw_condition <- tribble(
  ~condition, ~cohort,
  "Breast cancer", "Breast",
  "Prostate cancer", "Prostate",
  "Colorectal cancer", "CRC",
  "Non-small cell lung cancer nonsquamous", "NSCLC", # obviously needs later mapping
  "Pancreatic cancer", "Pancreas",
  "Colon cancer", "CRC",
  "Non-small cell lung cancer squamous", "NSCLC",
  "Bladder cancer", "Bladder",
  "Rectal cancer", "CRC",
  "Bladder CIS", "Bladder" # I know CIS probably doesn't merit inclusion.
)

bpc_drugs <- dft_hdrug_cohort %>%
  count(agent, sort = T) %>%
  mutate(
    found_in_ind = tolower(agent) %in% unique(tolower(dft_ind$component))
  )

exact_matches <- bpc_drugs %>%
  filter(found_in_ind) %>%
  select(agent) %>%
  mutate(component = agent)

strings_to_trim <- c(
  "Hydrochloride",
  "Calcium",
  "Citrate",
  "Disodium",
  "Acetate",
  "Dichloride",
  "Dimaleate",
  "Sulfate",
  "Hydrochloride",
  "Malate",
  "Mesylate",
  "Camsylate",
  "Phosphate",
  "Tartrate",
  "Smalate"
)

bpc_drugs %<>%
  mutate(
    agent_trim = str_replace_all(
      agent, 
      pattern = paste(strings_to_trim, collapse = "|"),
      replacement = ""
    ),
    agent_trim = str_trim(agent_trim)
  ) %>%
  mutate(
    trimmed_in_ind = tolower(agent_trim) %in% unique(tolower(dft_ind$component))
  ) 

trimmed_matches <- bpc_drugs %>%
  filter(!str_detect(agent, "Etoposide")) %>%
  filter(trimmed_in_ind & !found_in_ind) %>%
  mutate(component = agent_trim) %>%
  select(agent, component)
  
# For the ones that don't match exactly we'll do some work.

bpc_drugs %>% 
  filter(!found_in_ind & !trimmed_in_ind) %>% 
  select(agent, n) %>%
  print(n = 500) #%>% slice(1:10)

dft_ind %>% 
  filter(str_detect(tolower(component), "vedoliz")) %>%
  count(component, regulator) 

# The manually added ones:
#dft_cw_component <- tribble(
manual_matches <- tribble(
  ~component, ~agent, 
  "Paclitaxel nanoparticle albumin-bound", "Nabpaclitaxel",
  "Goserelin", "Goserlin Acetate",
  "Radium-223", "Radium RA 223 Dichloride",
  "Lapatinib", "Lapatinib Ditosylate",
  "Sipuleucel-T", "Sipuleucel T",
  "Ziv-aflibercept", "Ziv Aflibercept",
  "Sorafenib", "Sorafenib Tosylate",
  "Interferon alfa-2b", "Interferon"
  # Leaving this one behind:
#  "Interferon alfa-2b", "Recombinant Interferon Alfa",
)

dft_cw_drug <- rbind(
  mutate(exact_matches, agent_map_type = "exact"),
  mutate(trimmed_matches, agent_map_type = "trimmed"),
  mutate(manual_matches, agent_map_type = "manual")
)

if (dft_cw_drug %>%
    filter(duplicated(component)) %>%
    nrow()
) {
  cli_abort("Duplicate agent rows in drug mapping table")
}

dft_ind_converted <- dft_ind %>%
  filter(!is.na(component)) %>%
  filter(!(component %in% "NONE")) %>% # pending decision here
  filter(regulator %in% "FDA")

dft_ind_converted <- left_join(
  dft_ind_converted,
  dft_cw_drug,
  by = "component"
) %>%
  rename(mapped_agent = agent)

dft_ind_converted <- left_join(
  dft_ind_converted,
  dft_cw_condition,
  by = 'condition'
) %>%
  rename(mapped_cohort = cohort)

dft_ind_converted %<>%
  filter(!is.na(mapped_cohort) & !is.na(mapped_agent))

possible_approvals <- left_join(
  (dft_hdrug_cohort %>% 
     select(cohort, record_id, ca_seq, regimen_number, drug_number, agent)
  ),
  (dft_ind_converted %>%
     select(
       cohort = mapped_cohort,
       agent = mapped_agent,
       date,
       context, 
       stage_or_status,
       condition,
       agent_map_type
     ) %>%
     mutate(approval_exists = T)
  ),
  by = c('cohort', 'agent'),
  relationship = "many-to-many"
)

possible_approvals %<>% 
  mutate(
    approval_exists = if_else(
      is.na(approval_exists), 
      F, 
      approval_exists
      )
    )

# oooo messy.
dft_hdrug_app <- possible_approvals %>%
  # This is excessive to get uniqueness - I just want the drug and cohort for
  #   the sake of documentation.
  group_by(
    cohort, record_id, ca_seq, regimen_number, drug_number, agent
  ) %>%
  summarize(
    approved = sum(approval_exists, na.rm = T) >= 1,
    .groups = "drop"
  )

dft_hdrug_app %>%
  group_by(cohort, agent) %>%
  summarize(
    n_uses = n(),
    n_approved = sum(approved, na.rm = T),
    prop_approved = n_approved / n_uses,
    .groups = "drop"
  )
    
       
    
  


# Notes:

# - I did not add nabpaclitaxel since it appeared there were papers debating
#.  its equivalence to paclitaxel.
# - Wasn't really sure what to do with interferon.  I did the most common.
  
  
  

bpc_drugs <- count(dft_hdrug_cohort, agent, sort = T)

bpc_drugs %>%
  mutate(
    found_in_ind = agent %in% unique(dft_ind$component)
  )

dft_ind %>%
  filter(str_detect(component, "Fluorou"))

bpc_drugs
  
dft_ind %>% 
  filter(condition %in% "NONE") %>% 
  mutate(
    found_in_bpc_drugs = component %in% unique(bpc_drugs$agent)
  ) %>%
  # filter(found_in_bpc_drugs) %>%
  arrange(regulator, date) %>%
  View(.)

         
         