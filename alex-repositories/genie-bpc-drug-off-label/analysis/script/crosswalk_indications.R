library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_ind <- readr::read_csv(
  here('data-raw', 'manual', 'indications working copy.csv'),
  show_col_types = F
)

dft_cohort_cases <- readr::read_rds(
  here('data', 'cohort', 'clin_dat_wide.rds')
)

dft_hdrug_cohort <- dft_cohort_cases %>%
  select(cohort, hdrug) %>%
  unnest(hdrug)



# Fix some mistakes we noticed in the indications data.
# General issues:  OR vs | - both are used
# Brackets are sometimes used, sometimes not.
dft_ind %<>%
  mutate(
    with = case_when(
      # capital version was just most used:
      with %in% c("chemotherapy", "Chemotherapy") ~ "Chemotherapy",
      with %in% "Carboplatin AND Palictaxel" ~ 
                "Carboplatin AND Paclitaxel",
      with %in% "Carpoplatin AND (Paclitaxel OR nab-Paclitaxel)" ~
                "Carboplatin AND (Paclitaxel OR nab-Paclitaxel)",
      with %in% c("Aromatase inhibitor OR Tamoxifen",
                  "Tamoxifen|Aromatase inhibitor") ~
        "Aromatase inhibitor OR Tamoxifen", # OR is slightly more common than |
      with %in% c("Carboplatin and Pemetrexed") ~ "Carboplatin AND Pemetrexed",
      # This isn't strictly an error, but its inconsistent with the general
      #   convention to capitalize the first word.
      with %in% "trastuzumab" ~ "Trastuzumab",
      with %in% "not applicable" ~ NA_character_,
      T ~ with
    ),
    biomarker = case_when(
      biomarker %in% "not applicable" ~ NA_character_,
      biomarker %in% "no EGFR or ALK genomic tumor aberrations" ~ 
        "No EGFR mutations AND No ALK mutations",
      # This is lapatinib.  Clearly the letrozole indication, just has a typo
      #   in the study column.
      biomarker %in% "EGF30008" ~ "HR+ and HER2+",
      # from Nivolumab, assuming it's TPS based on the use in lung:
      biomarker %in% "PD-L1 at least 1%" ~ "PD-L1 TPS at least 1%",
      # Same rationale, these are from lung, just coded differently:
      biomarker %in% "PD-L1 at least 1% AND No EGFR mutations AND No ALK mutations" ~ "PD-L1 TPS at least 1% AND No EGFR mutations AND No ALK mutations",
      T ~ biomarker
    )
  )
  
  
      
  

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
  "Non-small cell lung cancer nonsquamous", "NSCLC", # obviously needs later mapping
  "Non-small cell lung cancer squamous", "NSCLC",
  "Non-small cell lung cancer", "NSCLC",
  "Pancreatic cancer", "Pancreas",
  "Colorectal cancer", "CRC",
  "Colon cancer", "CRC",
  "Rectal cancer", "CRC",
  "Bladder cancer", "Bladder",
  "Urothelial carcinoma", "Bladder"
)
 dft_cw_condition %<>% arrange(cohort, condition)


readr::write_rds(
  dft_cw_condition,
  here('data', 'warner_materials', 'cw_condition.rds')
)



# Now that we have the cohorts declared, we have to clean up one thing.
# "Malignant solid neoplasm" means any type of cancer as long as they
#   meet the biomarker requirements.
# We will populate one match for each of our cohorts to make this work.
cohorts_ind <- dft_cw_condition %>% 
  group_by(cohort) %>% slice(1) %>% pull(condition)

dft_pan_cancer_indications <- dft_ind %>%
  filter(condition %in% "Malignant solid neoplasm") %>%
  filter(regulator %in% "FDA") %>%
  # get rid of "dates" like "uncertain date" - can't do nothin with that.
  filter(str_detect(date, "^[0-9]{4}-[0-9]{2}")) %>%
  group_by(component) %>%
  arrange(date) %>%
  slice(rep(1, times = length(cohorts_ind))) %>%
  mutate(condition = cohorts_ind) %>%
  ungroup(.)

# Add those to the list:
dft_ind <- rbind(
  dft_ind,
  dft_pan_cancer_indications
)


# One alteration needed for the indications sheet:  Interferon is marked
#   in BPC as all one drug, and in the indications sheet it is separate 
#   interferons.  We decided in our January 2024 meeting to consider any approval
#   for interferon "good enough" in this project.  The easiest way to do that
#   is replacing the string:
dft_ind %<>%
  mutate(
    component = if_else(
      str_detect(tolower(component), 'interferon'),
      "Interferon", component
    )
  ) 

bpc_drugs <- dft_hdrug_cohort %>%
  count(agent, sort = T) %>%
  mutate(
    found_in_ind = tolower(agent) %in% tolower(dft_ind$component),
    exact_matching_str = dft_ind$component[
      match(x = tolower(agent), table = tolower(dft_ind$component))
    ]
  )

exact_matches <- bpc_drugs %>%
  filter(found_in_ind) %>%
  mutate(component = exact_matching_str) %>%
  select(agent, component)

bpc_drugs %<>% select(-exact_matching_str)

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
  # str_to_sentence decapitalizes the later words to match indications convention.
  mutate(component = str_to_sentence(agent_trim)) %>%
  select(agent, component)
  
# For the ones that don't match exactly we'll did some looking:
# bpc_drugs %>% 
#   filter(!found_in_ind & !trimmed_in_ind) %>% 
#   select(agent, n) %>%
#   print(n = 500) #%>% slice(1:10)
# dft_ind %>% 
#   filter(str_detect(tolower(component), "vedoliz")) %>%
#   count(component, regulator) 

# The manually added ones:
#dft_cw_component <- tribble(
manual_matches <- tribble(
  ~component, ~agent, 
  "Paclitaxel nanoparticle albumin-bound", "Nabpaclitaxel",
  "Goserelin", "Goserlin Acetate",
  # There's whitespace in Radium RA 223, but we fixed that in an earier step now.
  "Radium-223", "Radium RA 223 Dichloride",
  "Lapatinib", "Lapatinib Ditosylate",
  "Sipuleucel-T", "Sipuleucel T",
  "Ziv-aflibercept", "Ziv Aflibercept",
  "Sorafenib", "Sorafenib Tosylate",
  # "Interferon alfa-2b", "Interferon", # not needed with the fix above.
  "Tegafur gimeracil oteracil","Tegafurgimeraciloteracil Potassium",
  "Trastuzumab and hyaluronidase", "Trastuzumab/Hyaluronidase-oysk",
  "Pertuzumab and Trastuzumab hyaluronidase", "Pertuzumab-Trastuzumab-Hyaluronidase-ZZXF"
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


readr::write_csv(
  (bpc_drugs |>
     filter(!(agent %in% dft_cw_drug$agent)) |>
     select(agent, n)
   ),
  here('output', 'bpc_drugs_with_no_component_match.csv')
)


readr::write_rds(
  dft_cw_drug,
  here('data', 'warner_materials', 'cw_drug.rds')
)

# Write a copy of the full thing, just for formatting/location:
readr::write_rds(
  dft_ind,
  here('data', 'warner_materials', 'indications_full.rds')
)






# Map the columns we need in the indications over to the PRISSMM names:
dft_ind_mapped <- left_join(
  dft_ind,
  dft_cw_drug,
  by = "component"
) %>%
  rename(mapped_agent = agent)

dft_ind_mapped <- left_join(
  dft_ind_mapped,
  dft_cw_condition,
  by = 'condition'
) %>%
  rename(mapped_cohort = cohort)

readr::write_rds(
  dft_ind_mapped,
  here('data', 'warner_materials', 'indications_mapped.rds')
)




dft_ind_limited <- dft_ind_mapped %>%
  filter(!is.na(mapped_cohort) & !is.na(mapped_agent))

dft_ind_limited %<>%
  filter(!is.na(component)) %>%
  filter(!(component %in% "NONE")) %>%
  filter(regulator %in% "FDA")

readr::write_rds(
  dft_ind_limited,
  here('data', 'warner_materials', 'indications_mapped_limited.rds')
)







