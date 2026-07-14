# Description: Get sample sequenced for patients used in phase 1. 
# Author: Haley Hunter-Zinck
# Date: 2021-11-12

# pre-setup  ---------------------------

library(optparse)

waitifnot <- function(cond, msg) {
  if (!cond) {
    
    for (str in msg) {
      message(str)
    }
    message("Press control-C to exit and try again.")
    
    while(T) {}
  }
}

# user input ----------------------------

option_list <- list( 
  make_option(c("-c", "--cohort"), type = "character",
              help="BPC cohort"),
  make_option(c("-s", "--site"), type = "character",
              help="BPC site"),
  make_option(c("-u", "--save_synapse"), action="store_true", default = FALSE, 
              help="Save output to Synapse"),
  make_option(c("-p", "--perform_check"), action = "store_true", default = FALSE,
                help = "Perform triangulation check on sample list")
)
opt <- parse_args(OptionParser(option_list=option_list))
waitifnot(!is.null(opt$cohort) && !is.null(opt$site),
          msg = "Rscript template.R -h")

cohort <- opt$cohort
site <- opt$site
save_synapse <- opt$save_synapse
perform_check <- opt$perform_check

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(lubridate)
library(synapser)
synLogin()

# synapse
synid_table_bpc <- "syn21446700"
synid_table_bpc_sam <- "syn21446709"
synid_table_pat <- "syn7517669"
synid_table_sam <- "syn7517674"
synid_folder_exports <- "syn20781633"

# parameters
allowed_codes <- c('NSCLC', 'CMPT', 'LCLC', 'LUAD', 'LUPC', 'LUSC', 'NSCLCPD', 'LUAS', 'LUMEC', 'LECLC', 'CCLC', 'RLCLC', 'GCLC', 'BLCLC')
seq_min <- "Jan-2018"
seq_max <- "Sep-2021"

# functions ----------------------------

is_double_value <- function(x) {
  res <- tryCatch({
    as.double(x)
  }, error = function(cond){
    return(NA)
  }, warning = function(cond){
    return(NA)
  }, finally = {
  })
  
  if (is.na(res)) {
    return(F)
  }
  return(T)
}

is_double <- function(x) {
  return(apply(as.matrix(x), 1, is_double_value))
}

#' Create data matrix with all necessary information to determine 
#' eligibility for BPC cohort case selection. 
#' 
#' @param patient Data from the data_clinical_patient.txt file from a GENIE
#'                consortium release
#' @param sample Data from the data_clinical_sample.txt file from a GENIE
#'                consortium release
#' @return Matrix of data elements for all samples in the consortium data files.
#' @example 
#'   get_eligibility_data(patient, sample)
get_eligibility_data <- function(synid_table_patient, synid_table_sample, site) {
  
  # read table data
  patient_data <- as.data.frame(synTableQuery(query = glue("SELECT PATIENT_ID, 
                                                                  CENTER,
                                                                  YEAR_DEATH
                                                                  FROM {synid_table_patient}"),
                                              includeRowIdAndRowVersion = F)) 
  sample_data <- as.data.frame(synTableQuery(query = glue("SELECT PATIENT_ID, 
                                                                  SAMPLE_ID,
                                                                  ONCOTREE_CODE,
                                                                  SEQ_DATE,
                                                                  SEQ_YEAR,
                                                                  AGE_AT_SEQ_REPORT
                                                                  FROM {synid_table_sample}"),
                                             includeRowIdAndRowVersion = F)) 
  
  # merge and filter
  data <- patient_data %>% 
    inner_join(sample_data, by = "PATIENT_ID") %>%  
    filter(CENTER == site) %>%
    select(PATIENT_ID, 
           SAMPLE_ID, 
           ONCOTREE_CODE, 
           SEQ_DATE, 
           AGE_AT_SEQ_REPORT,
           SEQ_YEAR,
           YEAR_DEATH)
  
  return(data)
}

#' Create a matrix of both data and flags for exclusion criteria for all
#' samples in GENIE for eligibility for BPC cohort.  
#' 
#' @param data matrix of all necessary data elements for each sample in order
#'             to determine eligibility.
#' @param allowed_codes character vector eligible OncoTree codes
#' @param seq_min earliest eligible sequencing date (format: %b-%Y)
#' @param seq_min latest eligible sequencing date (format: %b-%Y)
#' @return Matrix of data elements used to determine eligibility and flags indicating 
#'   inclusion or exclusion for a given eligibility criteria check.
#' @example
#' create_eligibility_matrix(data = get_eligibility_data(patient, sample))
create_eligibility_matrix <- function(data, 
                                      allowed_codes, 
                                      seq_min, 
                                      seq_max,
                                      exclude_patient_id = c(),
                                      exclude_sample_id = c()) {
  
  mat <- data %>% 
    
    # valid oncotree code
    mutate(FLAG_ALLOWED_CODE = is.element(ONCOTREE_CODE, allowed_codes)) %>%   
    
    # >=18 years old at sequencing
    mutate(FLAG_ADULT = AGE_AT_SEQ_REPORT != '<6570') %>%            
    
    # sequenced within specified time range
    mutate(FLAG_SEQ_DATE = my(SEQ_DATE) >= my(seq_min) & my(SEQ_DATE) <= my(seq_max)) %>%
    
    # patient was alive at sequencing
    mutate(FLAG_SEQ_ALIVE = !is_double(YEAR_DEATH) | YEAR_DEATH >= SEQ_YEAR)  %>% 
    
    # patient not explicitly excluded
    mutate(FLAG_NOT_EXCLUDED = !is.element(PATIENT_ID, exclude_patient_id) & !is.element(SAMPLE_ID, exclude_sample_id))  %>% 
    
    select(PATIENT_ID, 
           SAMPLE_ID, 
           ONCOTREE_CODE, 
           AGE_AT_SEQ_REPORT,
           SEQ_DATE,
           SEQ_YEAR,
           YEAR_DEATH,
           FLAG_ALLOWED_CODE, 
           FLAG_ADULT, 
           FLAG_SEQ_DATE, 
           FLAG_SEQ_ALIVE,
           FLAG_NOT_EXCLUDED)         
  
  return(mat)
}

#' Get patient ID, and group relevant sample IDs, of the eligible cohort.  
#' 
#' @param x Eligibility matrix.
#' @param randomize if TRUE, cohort should be randomly shuffled; 
#'                  if FALSE, return order as is
#' @return Matrix of patient and sample ID pairs.  
get_eligible_cohort <- function(x, randomize = F) {
  
  mod <- x
  
  col_flags <- grep(pattern = "^FLAG_", x = colnames(x), value = T, invert = F)
  mod$flag_eligible <- apply(x[,col_flags], 1, all)
  
  # determine eligible samples (all flags TRUE)
  eligible <- as.data.frame(mod %>%
                              filter(flag_eligible) %>% 
                              group_by(PATIENT_ID) %>%
                              summarize(SAMPLE_IDS = paste0(SAMPLE_ID, collapse = ";")) %>%
                              select(PATIENT_ID, SAMPLE_IDS))
  
  # randomize cohort
  final <- list()
  if (randomize) {
    final <- eligible %>%
      sample_n(size = nrow(eligible)) %>%
      mutate(order = c(1:nrow(eligible))) %>%
      select(order, PATIENT_ID, SAMPLE_IDS)
  } else {
    final <- eligible %>%
      mutate(order = c(1:nrow(eligible))) %>%
      select(order, PATIENT_ID, SAMPLE_IDS) 
  }
  
  return(final)
}

get_synapse_folder_children <- function(synapse_id, 
                                        include_types=list("folder", "file", "table", "link", "entityview", "dockerrepo")) {
  
  ent <- as.list(synGetChildren(synapse_id, includeTypes = include_types))
  
  children <- c()
  
  if (length(ent) > 0) {
    for (i in 1:length(ent)) {
      children[ent[[i]]$name] <- ent[[i]]$id
    }
  }
  
  return(children)
}

#' Get a synapse ID by following a traditional file path from a root synapse folder entity.
#' 
#' @param synid_folder_root Synapse ID of the root folder
#' @param path Folder path starting in the first subfolder ending in desired folder and delimited with '/'
#' @return Synapse ID of the final subfolder in the path
#' @example get_folder_synid_from_path("syn12345", "first/second/final")
get_folder_synid_from_path <- function(synid_folder_root, path) {
  
  synid_folder_current <- synid_folder_root
  subfolders <- strsplit(path, split = "/")[[1]]
  
  for (i in 1:length(subfolders)) {
    synid_folder_children <- get_synapse_folder_children(synid_folder_current, 
                                                         include_types = list("folder"))
    
    if (!is.element(subfolders[i], names(synid_folder_children))) {
      return(NA)
    }
    
    synid_folder_current <- as.character(synid_folder_children[subfolders[i]])
  }
  
  return(synid_folder_current)
}

#' Store a file on Synapse with options to define provenance.
#' 
#' @param path Path to the file on the local machine.
#' @param parent_id Synapse ID of the folder or project to which to load the file.
#' @param file_name Name of the Synapse entity once loaded
#' @param prov_name Provenance short description title
#' @param prov_desc Provenance long description
#' @param prov_used Vector of Synapse IDs of data used to create the current
#' file to be loaded.
#' @param prov_exec String representing URL to script used to create the file.
#' @return TRUE if successful, otherwise return error.
save_to_synapse <- function(path, 
                            parent_id, 
                            file_name = NA, 
                            prov_name = NA, 
                            prov_desc = NA, 
                            prov_used = NA, 
                            prov_exec = NA) {
  
  if (is.na(file_name)) {
    file_name = path
  } 
  file <- File(path = path, parentId = parent_id, name = file_name)
  
  if (!is.na(prov_name) || !is.na(prov_desc) || !is.na(prov_used) || !is.na(prov_exec)) {
    act <- Activity(name = prov_name,
                    description = prov_desc,
                    used = prov_used,
                    executed = prov_exec)
    file <- synStore(file, activity = act)
  } else {
    file <- synStore(file)
  }
  
  return(T)
}

# read ----------------------------

query <- glue("SELECT record_id AS PATIENT_ID FROM {synid_table_bpc} WHERE cohort = '{cohort}' AND redcap_data_access_group = '{site}'")
bpc_pat_ids <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))

# main ----------------------------

eligibility_data <- get_eligibility_data(synid_table_patient = synid_table_pat, 
                                         synid_table_sample = synid_table_sam, 
                                         site = site)
eligibility_matrix <- create_eligibility_matrix(data = eligibility_data, 
                                                allowed_codes = allowed_codes, 
                                                seq_min = seq_min, 
                                                seq_max = seq_max,
                                                exclude_patient_id = c(),
                                                exclude_sample_id = c())
eligible_cohort <- get_eligible_cohort(x = eligibility_matrix, randomize = T) 


added_sam <- eligible_cohort %>% 
  filter(is.element(PATIENT_ID, unlist(bpc_pat_ids))) %>%
  select(PATIENT_ID, SAMPLE_IDS)

added_sam$ALREADY_IN_BPC <- rep(F, nrow(added_sam))
if (nrow(added_sam)) {
  for (i in 1:nrow(added_sam)) {
    ids_sam <- added_sam[i, "SAMPLE_IDS"]
    str_ids_sam <- paste0("'", paste0(unlist(strsplit(ids_sam, split = ";")), collapse = "','"), "'")
    query <- glue("SELECT cpt_genie_sample_id FROM {synid_table_bpc_sam} WHERE cpt_genie_sample_id IN ({str_ids_sam})")
    res <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))
    if (nrow(res)) {
      added_sam$ALREADY_IN_BPC[i] <- T
    }
  }
} 

# checks --------------

if (perform_check && nrow(added_sam)) {
  check_labels <- c("pat_in_bpc", "sam_not_bpc", "sam_in_mg")
  mat_check <- matrix(F, nrow = nrow(added_sam), ncol = 3, dimnames = list(c(), check_labels))
  
  for (i in 1:nrow(added_sam)) {
    
    id_pat <- added_sam[i, "PATIENT_ID"]
    ids_sam <- added_sam[i, "SAMPLE_IDS"]
    str_ids_sam <- paste0("'", paste0(unlist(strsplit(ids_sam, split = ";")), collapse = "','"), "'")
    
    # check patient id is in bpc
    query <- glue("SELECT record_id FROM {synid_table_bpc} WHERE record_id = '{id_pat}'")
    res <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))
    if (length(res)) {
      mat_check[i, "pat_in_bpc"] <- T
    }
    
    # check sample --is not-- in bpc table
    query <- glue("SELECT cpt_genie_sample_id FROM {synid_table_bpc_sam} WHERE cpt_genie_sample_id IN ({str_ids_sam})")
    res <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))
    if (!nrow(res)) {
      mat_check[i, "sam_not_bpc"] <- T
    }
    
    # check sample --is-- in main genie table
    query <- glue("SELECT SAMPLE_ID FROM {synid_table_sam} WHERE SAMPLE_ID IN ({str_ids_sam})")
    res <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))
    if (nrow(res)) {
      mat_check[i, "sam_in_mg"] <- T
    }
  }
  
  if (length(which(!c(mat_check)))) {
    print("warning: didn't pass at least one check")
  } else {
    print("pass: all checks passed!")
  }
} 

# write ----------------------------

file_output <- tolower(glue("{cohort}_{site}_phase1_additional_samples.csv"))
write.csv(added_sam, file = file_output, row.names = F)

if (save_synapse) {
  parent_id <- get_folder_synid_from_path(synid_folder_root = synid_folder_exports,
                                          path = glue("{cohort}/{site}"))
  save_to_synapse(path = file_output, 
                  parent_id = parent_id, 
                  prov_name = "updated sample list", 
                  prov_desc = glue("additional samples for NSCLC phase 1 cohorts seqeuenced between {seq_min} and {seq_max}"), 
                  prov_used = c(synid_table_bpc, synid_table_pat, synid_table_sam), 
                  prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2021-11-12_shawn_nsclc_phase1_samples.R")
  file.remove(file_output)
}

# close out ----------------------------

print(glue("Cohort: {cohort}"))
print(glue("Site: {site}"))
print(glue("Number of patients with additional samples: {nrow(added_sam)}"))

if (save_synapse) {
  print(glue("Output saved to Synaspe as '{file_output}' in folder '{synGet(parent_id)$properties$name}' ({parent_id})"))
} else {
  print(glue("Output written locally to '{file_output}'"))
}

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
