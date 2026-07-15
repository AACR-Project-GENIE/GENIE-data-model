# This script declares function arguments to be run with 
#   add_check_with_simple().
# These are concomitant drug requirements (and monotherapy), noted
#   using the 'with' column in the HemOnc sheet.

library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_simple_with_tests <- tibble(
  with_req = character(0),
  # agent_req can be length >= 2, so we need a list column:
  agent_req = list(character(0)),
  test_name = character(0)
)

dft_simple_with_tests %<>%
  add_row(
    with_req = "5-FU-based regimen",
    agent_req = list("Fluorouracil")
  ) %>%
  add_row(
    with_req = "Cisplatin",
  ) %>%
  add_row(
    with_req = "Docetaxel"
  ) %>%
  add_row(
    with_req = "Gemcitabine",
    agent_req = list("Gemcitabine Hydrochloride"),
    test_name = "test_with_gemcitabine",
  ) %>%
  add_row(
    with_req = "Letrozole"
  ) %>%
  add_row(
    with_req = "Fulvestrant"
  ) %>%
  add_row(
    with_req = "Abemaciclib"
  ) %>%
  add_row(
    with_req = "Palbociclib"
  ) %>%
  add_row(
    with_req = "Ribociclib"
  ) %>%
  add_row(
    with_req = "Ipilimumab"
  ) %>%
  add_row(
    with_req = "Irinotecan",
    agent_req = list("Irinotecan Hydrochloride"), # Add Irinotecan liposome?
    test_name = "test_with_irinotecan"
  ) %>%
  add_row(
    with_req = "Capecitabine",
  ) %>%
  add_row(
    with_req = "Bevacizumab",
  ) %>%
  add_row(  
    with_req = "Exemestane",
  ) %>%
  add_row(
    with_req = "Erlotinib",
    agent_req = list("Erlotinib Hydrochloride"),
    test_name = "test_with_erlotinib"
  ) %>%
  add_row(  
    with_req = "Carboplatin"
  ) %>%
  add_row(
    with_req = "Dabrafenib"
  ) %>%
  add_row(    
    with_req = "Trametinib"
  ) %>%
  add_row(  
    with_req = "Nivolumab"
  ) %>%
  add_row(
    with_req = "Pembrolizumab"
  ) %>%
  add_row(
    with_req = "Cetuximab"
  ) %>%
  add_row(
    with_req = "Encorafenib"
  ) %>%
  add_row(
    with_req = "Paclitaxel"
  )

dft_simple_with_tests %<>%
  add_row(
    with_req = "Doxorubicin AND Cyclophosphamide AND Paclitaxel",
    agent_req = list(c(
      "Doxorubicin Hydrochloride",
      "Cyclophosphamide",
      "Paclitaxel"
    )),
    # For these combo regimens I'm going to give them new names because
    #   the auto generated variable names get too long.
    test_name = "test_with_doxil_cyclo_pac"
  )

dft_simple_with_tests %<>%
  add_row(
    with_req = "Carboplatin AND Pembrolizumab",
    agent_req = list(c(
      "Carboplatin",
      "Pembrolizumab"
    )),
    test_name = "test_with_carbo_pembro"
  )

dft_simple_with_tests %<>%
  add_row(
    with_req = "Carboplatin AND Paclitaxel",
    agent_req = list(c(
      "Carboplatin",
      "Paclitaxel"
    )),
    test_name = "test_with_carbotaxol"
  )

dft_simple_with_tests %<>%
  add_row(
    with_req = "Carboplatin AND Pemetrexed",
    agent_req = list(c(
      "Carboplatin",
      "Pemetrexed"
    )),
    test_name = "test_with_carbopem"
  )

dft_simple_with_tests %<>%
  add_row(
    with_req = "FOLFIRI",
    agent_req = list(c(
      "Leucovorin Calcium",
      "Fluorouracil",
      "Irinotecan Hydrochloride"
    )),
    test_name = "test_with_folfiri"
  )

dft_simple_with_tests %<>%
  add_row(
    with_req = "FOLFOX",
    agent_req = list(c(
      "Leucovorin Calcium",
      "Fluorouracil",
      "Oxaliplatin"
    )),
    test_name = "test_with_folfox"
  )

dft_simple_with_tests %<>%
  add_row(
    with_req = "Fluorouracil AND Folinic acid",
    agent_req = list(c(
      "Fluorouracil",
      "Leucovorin Calcium"
    )),
    test_name = "test_with_folf" # fol = folinic acid = leucovorin
  )

dft_simple_with_tests %<>%
  add_row(
    with_req = "Paclitaxel, nanoparticle albumin-bound AND Carboplatin",
    agent_req = list(c(
      "Nabpaclitaxel",
      "Carboplatin"
    )),
    test_name = "test_with_carbo_nabpac" 
  )

# We can't check prednisone in BPC (not collected) so we just do Abi here.
dft_simple_with_tests %<>%
  add_row(
    with_req = "Abiraterone AND (Prednisone|Prednisolone)",
    agent_req = list(c(
      "Abiraterone Acetate"
    )),
    test_name = "test_with_abiraterone" 
  )

dft_simple_with_tests %<>%
  add_row(
    with_req = "Cisplatin AND Gemcitabine",
    agent_req = list(c(
      "Cisplatin",
      "Gemcitabine Hydrochloride"
    )),
    test_name = "test_with_gem_cis" 
  )

dft_simple_with_tests %<>%
  add_row(
    with_req = "Bevacizumab AND Paclitaxel AND Carboplatin",
    agent_req = list(c(
      "Bevacizumab",
      "Paclitaxel",
      "Carboplatin"
    )),
    test_name = "test_with_bev_pac_carbo"
  )

dft_simple_with_tests %<>%
  add_row(
    with_req = "Trastuzumab AND Docetaxel",
    agent_req = list(c(
      "Trastuzumab",
      "Docetaxel"
    )),
    test_name = "test_with_trast_doce"
  )

readr::write_rds(
  dft_simple_with_tests,
  file = here('data', 'linked_approvals', 'simple_with_tests.rds')
)


