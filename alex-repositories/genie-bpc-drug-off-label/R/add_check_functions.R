add_check_met <- function(
    dat_poss_app
) {
  dat_poss_app %<>%
    mutate(
      test_met = case_when(
        # This check only really applies when we have metastatic.
        # For the moment we're ignoring "advanced or metastatic" and similar.
        is.na(stage_or_status) ~ T, 
        !(stage_or_status %in% "Metastatic") ~ T,
        dmet_at_drug_start ~ T,
        T ~ F
      )
    )
  return(dat_poss_app)
}

add_check_monotherapy <- function(
    dat_poss_app
) {
  dat_poss_app %<>%
    mutate(
      test_with_nothing = case_when(
        is.na(ind_with) ~ T, 
        !(ind_with %in% "0") ~ T, # 0 = monotherapy required.
        num_overlaps %in% 0 ~ T,
        T ~ F
      )
    )
  return(dat_poss_app)
}

# This just checks that they have at least one drug.  Obviously all drugs
#   are not chemotherapy, so this is conservative in declaring violations.
add_check_chemo <- function(
    dat_poss_app
) {
  dat_poss_app %<>%
    mutate(
      test_with_chemo = case_when(
        is.na(ind_with) ~ T, 
        !(ind_with %in% "Chemotherapy") ~ T, 
        num_overlaps >= 1 ~ T,
        T ~ F
      )
    )
  return(dat_poss_app)
}

platinum_helper <- function(
    dat_poss_app
) {
  d1_list <- c("Oxaliplatin", "Cisplatin", "Carboplatin")
  
  dat_poss_app %<>%
    mutate(
      .has_drug_1 = map_lgl(
        .x = drug_overlaps,
        .f = \(z) any(d1_list %in% z)
      )
    )
  
  return(dat_poss_app)
}

add_check_plat <- function(
    dat_poss_app
) {
  dat_poss_app %<>%
    platinum_helper %>%
    mutate(
      test_with_plat = case_when(
        is.na(ind_with) ~ T, 
        !(ind_with %in% "Platinum-containing chemotherapy") ~ T, 
        .has_drug_1 ~ T,
        T ~ F
      )
    )
  
  dat_poss_app %<>% select(-matches("^\\.has_drug_1"))
  
  return(dat_poss_app)
}
  

# Adds in the drug_1 and drug_2 flags for platinum doublets.  Re-used enough
#   to be its own function.
platinum_doublet_helper <- function(
    dat_poss_app
) {
  doublet_list <- c("Paclitaxel", "Docetaxel", "Gemcitabine Hydrochloride",
                    "Vinorelbine", "Irinotecan", "Pemetrexed Disodium",
                    "Tegafurgimeraciloteracil Potassium")
  
  dat_poss_app %<>%
    platinum_helper(.) %>% # Adds drug_1 as platinum.
    mutate(
      .has_drug_2 = map_lgl(
        .x = drug_overlaps,
        .f = \(z) any(doublet_list %in% z)
      )
    )
  
  return(dat_poss_app)
  
}

add_check_plat_doublet <- function(
    dat_poss_app
) {
  dat_poss_app %<>%
    platinum_doublet_helper %>%
    mutate(
      test_with_plat_doublet = case_when(
        is.na(ind_with) ~ T, 
        !(ind_with %in% "Platinum-doublet") ~ T, 
        .has_drug_1 & .has_drug_2 ~ T,
        T ~ F
      )
    )
  
  dat_poss_app %<>% select(-matches("^\\.has_drug"))
  
  return(dat_poss_app)
}


add_check_ipilum_pd <- function(
    dat_poss_app
) {
  ipi_list <- c("Ipilimumab")
  
  dat_poss_app %<>%
    platinum_doublet_helper %>%
    mutate(
      .has_drug_3 = map_lgl(
        .x = drug_overlaps,
        .f = \(z) any(ipi_list %in% z)
      )
    ) %>%
    mutate(
      test_with_ipilum_pd = case_when(
        is.na(ind_with) ~ T, 
        !(ind_with %in% "Ipilimumab AND Platinum doublet") ~ T, 
        .has_drug_1 & .has_drug_2 & .has_drug_3 ~ T,
        T ~ F
      )
    )
  
  dat_poss_app %<>% select(-matches("^\\.has_drug"))
  
  return(dat_poss_app)
}

add_check_carbotaxol_nab <- function(
    dat_poss_app
) {
  d1_list <- c("Carboplatin")
  d2_list <- c("Nabpaclitaxel", "Paclitaxel")
  
  dat_poss_app %<>%
    mutate(
      .has_drug_1 = map_lgl(
        .x = drug_overlaps,
        .f = \(z) any(d1_list %in% z)
      ),
      .has_drug_2 = map_lgl(
        .x = drug_overlaps,
        .f = \(z) any(d2_list %in% z)
      )
    ) %>%
    mutate(
      test_with_carbotaxol_nab = case_when(
        is.na(ind_with) ~ T, 
        !(ind_with %in% "Carboplatin AND (Paclitaxel OR nab-Paclitaxel)") ~ T, 
        .has_drug_1 & .has_drug_2 ~ T,
        T ~ F
      )
    )
  
  dat_poss_app %<>% select(-matches("^\\.has_drug"))
  
  return(dat_poss_app)
}

add_check_pem_plat <- function(
    dat_poss_app
) {
  d2_list <-  "Pemetrexed Disodium"
  
  dat_poss_app %<>%
    platinum_helper(.) %>%
    mutate(
      .has_drug_2 = map_lgl(
        .x = drug_overlaps,
        .f = \(z) any(d2_list %in% z)
      )
    ) %>%
    mutate(
      test_with_pem_plat = case_when(
        is.na(ind_with) ~ T, 
        !(ind_with %in% "Pemetrexed AND Platinum agent") ~ T, 
        .has_drug_1 & .has_drug_2 ~ T,
        T ~ F
      )
    )
  
  dat_poss_app %<>% select(-matches("^\\.has_drug"))
  
  return(dat_poss_app)
}

add_check_tras_chemo <- function(
    dat_poss_app
) {
  d1_list <- "Trastuzumab"
  # chemo is handled below with num_overlaps (relaxed check)
  
  dat_poss_app %<>%
    mutate(
      .has_drug_1 = map_lgl(
        .x = drug_overlaps,
        .f = \(z) any(d1_list %in% z)
      )
    ) %>%
    mutate(
      test_with_tras_chemo = case_when(
        is.na(ind_with) ~ T, 
        !(ind_with %in% "Trastuzumab AND Chemotherapy") ~ T, 
        .has_drug_1 & num_overlaps >= 2 ~ T,
        T ~ F
      )
    )
  
  dat_poss_app %<>% select(-matches("^\\.has_drug"))
  
  return(dat_poss_app)
}


aromatase_helper <- function(
    dat_poss_app
) {
  d1_list <- c("Anastrozole", "Letrozole", "Exemestane")
  
  dat_poss_app %<>%
    mutate(
      .has_drug_1 = map_lgl(
        .x = drug_overlaps,
        .f = \(z) any(d1_list %in% z)
      )
    )
  
  return(dat_poss_app)
}

add_check_ai <- function(
    dat_poss_app
) {
  dat_poss_app %<>%
    aromatase_helper %>%
    mutate(
      test_with_ai = case_when(
        is.na(ind_with) ~ T, 
        !(ind_with %in% "Aromatase inhibitor") ~ T, 
        .has_drug_1 ~ T,
        T ~ F
      )
    )
  
  dat_poss_app %<>% select(-matches("^\\.has_drug"))
  
  return(dat_poss_app)
}

add_check_ai_or_tamox <- function(
    dat_poss_app
) {
  d2_list <- "Tamoxifen Citrate"
  
  dat_poss_app %<>%
    aromatase_helper %>%
    mutate(
      .has_drug_2 = map_lgl(
        .x = drug_overlaps,
        .f = \(z) any(d2_list %in% z)
      )
    ) %>%
    mutate(
      test_with_ai_tamox = case_when(
        is.na(ind_with) ~ T, 
        !(ind_with %in% "Aromatase inhibitor OR Tamoxifen") ~ T, 
        .has_drug_1 | .has_drug_2 ~ T,
        T ~ F
      )
    )
  
  dat_poss_app %<>% select(-matches("^\\.has_drug"))
  
  return(dat_poss_app)
}


add_check_pd_nivo <- function(
    dat_poss_app
) {
  d2_list <- "Nivolumab"
  
  dat_poss_app %<>%
    platinum_doublet_helper %>%
    mutate(
      .has_drug_2 = map_lgl(
        .x = drug_overlaps,
        .f = \(z) any(d2_list %in% z)
      )
    ) %>%
    mutate(
      test_with_pd_nivo = case_when(
        is.na(ind_with) ~ T, 
        !(ind_with %in% "Nivolumab AND Platinum doublet") ~ T, 
        .has_drug_1 | .has_drug_2 ~ T,
        T ~ F
      )
    )
  
  dat_poss_app %<>% select(-matches("^\\.has_drug"))
  
  return(dat_poss_app)
}


# A and B or A and C = A and (B or C)
add_check_fluor_iri_or_oxal<- function(
    dat_poss_app
) {
  # https://go.drugbank.com/categories/DBCAT003851
  d1_list <- c("Capecitabine", "Fluorouracil", "Tegafurgimeraciloteracil Potassium")
  d2_list <- c("Irinotecan Hydrochloride", "Oxaliplatin")
  
  dat_poss_app %<>%
    platinum_doublet_helper %>%
    mutate(
      .has_drug_1 = map_lgl(
        .x = drug_overlaps,
        .f = \(z) any(d1_list %in% z)
      ),
      .has_drug_2 = map_lgl(
        .x = drug_overlaps,
        .f = \(z) any(d2_list %in% z)
      )
    ) %>%
    mutate(
      test_with_fluor_iri_or_oxal = case_when(
        is.na(ind_with) ~ T, 
        !(ind_with %in% "Fluoropyrimidine AND Irinotecan OR Fluoropyrimidine AND Oxaliplatin") ~ T, 
        .has_drug_1 & .has_drug_2 ~ T,
        T ~ F
      )
    )
  
  dat_poss_app %<>% select(-matches("^\\.has_drug"))
  
  return(dat_poss_app)
}





add_check_date_definite <- function(
    dat_poss_app
) {
  dat_poss_app %<>%
    mutate(
      test_date_definite = case_when(
        drug_start_date_max < ind_date ~ F,
        T ~ T
      )
    )
  return(dat_poss_app)
}

add_check_date_possible <- function(
    dat_poss_app
) {
  
  dat_poss_app %<>%
    mutate(
      test_date_possible = case_when(
        drug_start_date_min < ind_date ~ F,
        T ~ T
      )
    )
  return(dat_poss_app)
}


