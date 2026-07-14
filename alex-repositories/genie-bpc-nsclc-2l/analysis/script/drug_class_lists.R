library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

bpc_dat_path <- path('data-raw', 'NSCLC', '3.1-consortium')
reg <- readr::read_csv(path(bpc_dat_path, 'regimen_cancer_level_dataset.csv'))

drug_exp <- reg %>%
  select(record_id, ca_seq, regimen_number, matches('^drugs_drug')) %>%
  pivot_longer(matches('drugs_drug'), values_to = 'drug_name') %>%
  filter(!is.na(drug_name)) %>%
  count(drug_name, sort = T)

cat(drug_exp$drug_name, file = 'temp_drug_names.txt', sep = '\n')

# huge ai list:
# chemo_strings <- c(
#   "arsenic",
#   "azacitidine",
#   "bendamustine",
#   "bleomycin",
#   "busulfan",
#   "cabazitaxel",
#   "capecitabine",
#   "carboplatin",
#   "chlorambucil",
#   "cisplatin",
#   "cyclophosphamide",
#   "cytarabine",
#   "dacarbazine",
#   "dactinomycin",
#   "daunorubicin",
#   "decitabine",
#   "docetaxel",
#   "doxorubicin",
#   "epirubicin",
#   "etoposide",
#   "floxuridine",
#   "fludarabine",
#   "fluorouracil",
#   "gemcitabine",
#   "hydroxyurea",
#   "ifosfamide",
#   "irinotecan",
#   "ixabepilone",
#   "liposome",
#   "lomustine",
#   "lurbinectedin",
#   "mechlorethamine",
#   "melphalan",
#   "methotrexate",
#   "mitomycin",
#   "nabpaclitaxel",
#   "oxaliplatin",
#   "paclitaxel",
#   "pegylated",
#   "pemetrexed",
#   "procarbazine",
#   "temozolomide",
#   "thiotepa",
#   "vinblastine",
#   "vincristine",
#   "vinorelbine"
# )
chemo_strings <- c(
  "carboplatin",
  "cisplatin",
  "docetaxel",
  "oxaliplatin",
  "paclitaxel"
)
anti_pd1_strings <- c(
  'pembrolizumab',
  'nivolumab',
  'atezolizumab',
  'durvalumab'
)

drug_exp <- drug_exp %>%
  mutate(
    .drug_lower = tolower(drug_name),
    is_chemo = str_detect(.drug_lower, paste(chemo_strings, collapse = '|')),
    is_anti_pd1 = str_detect(
      .drug_lower,
      paste(anti_pd1_strings, collapse = '|')
    )
  ) %>%
  select(-.drug_lower)

readr::write_rds(
  drug_exp,
  here('data', 'drug_class.rds')
)

# Then tag the regimens as well:
reg_class <- reg %>%
  select(cohort, record_id, ca_seq, regimen_number, regimen_drugs) %>%
  mutate(
    .reg_lower = tolower(regimen_drugs),
    is_chemo = str_detect(.reg_lower, paste(chemo_strings, collapse = '|')),
    is_anti_pd1 = str_detect(
      .reg_lower,
      paste(anti_pd1_strings, collapse = '|')
    )
  ) %>%
  select(-.reg_lower)

readr::write_rds(
  reg_class,
  here('data', 'reg_class.rds')
)

readr::write_rds(
  list(chemo = chemo_strings, anti_pd1 = anti_pd1_strings),
  here('data', 'drug_string_matches.rds')
)
