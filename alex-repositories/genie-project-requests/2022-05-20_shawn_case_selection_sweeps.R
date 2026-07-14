# Description: Get number of eligible samples for each site by min date for a cohort.
# Author: Haley Hunter-Zinck
# Date: 2022-05-20

# cli --------------------

library(optparse)

#' Function to wait indefinitely upon a certain condition.
#' 
#' @param cond Boolean value, usually from an evaluated condition
#' @param msg A string to print upon condition being TRUE
#' @return NA
waitifnot <- function(cond, msg = "") {
  if (!cond) {
    
    for (str in msg) {
      message(str)
    }
    message("Press control-C to exit and try again.")
    
    while(T) {}
  }
}

option_list <- list( 
  make_option(c("-c", "--cohort"), type = "character",
              help="Phase 2 cohort label"),
  make_option(c("-o", "--synid_folder_output"), type = "character", default = NA,
              help="Synapse ID of output folder (default: write locally only)"),
  make_option(c("-v", "--verbose"), action="store_true", default = FALSE, 
              help="Output script messages to the user.")
)
opt <- parse_args(OptionParser(option_list=option_list))
waitifnot(!is.null(opt$cohort),
          msg = "Rscript 2022-05-20_shawn_case_selection_sweeps.R -h")

cohort <- opt$cohort
synid_folder_output <- opt$synid_folder_output
verbose <- opt$verbose

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(yaml)
library(dplyr)
library(lubridate)
library(synapser)

# parameters
url <- "https://raw.githubusercontent.com/Sage-Bionetworks/genie-bpc-pipeline/gh-66-phase2-case-selection/scripts/case_selection/config.yaml"
file_config <- "config.yaml"
phase <- "2"
outfile <- tolower(glue("case_selection_sweep_{phase}_{cohort}.csv"))

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

get_min_seq_date <- function(synid_table_sample, posix = F) {
  query <- glue("SELECT MIN(SEQ_DATE)  AS min_seq_date FROM {synid_table_sample}  WHERE SEQ_DATE NOT IN ('Release')")
  res <- as.character(unlist(as.data.frame(synTableQuery(query))))
  
  if (posix) {
    res <- as.POSIXct(gsub(pattern = "-", replacement = " 1 ", res), format = "%b %d %Y")
  }
  return(res)
}

get_max_seq_date <- function(synid_table_sample, posix = F) {
  query <- glue("SELECT MAX(SEQ_DATE)  AS max_seq_date FROM {synid_table_sample}  WHERE SEQ_DATE NOT IN ('Release')")
  res <- as.character(unlist(as.data.frame(synTableQuery(query))))
  
  if (posix) {
    res <- as.POSIXct(gsub(pattern = "-", replacement = " 1 ", res), format = "%b %d %Y")
  }
  return(res)
}

seq_date_to_posix <- function(seq_date) {
 return(as.POSIXct(gsub(pattern = "-", replacement = " 1 ", seq_date), format = "%b %d %Y")) 
}

get_month_dates <- function(seq_min_date, seq_max_date) {
  
  my_dates <- c()
  
  year_min <- as.integer(format(seq_min_date, "%Y"))
  year_max <- as.integer(format(seq_max_date, "%Y"))
  
  month_min <- as.integer(format(seq_min_date, "%m"))
  month_max <- as.integer(format(seq_max_date, "%m"))
  
  for (year in year_min:year_max) {
    
    month_min_itr <- 1
    month_max_itr <- 12
    if (year == year_min) {
      if (year_min == year_max) {
        month_min_itr <- month_min
        month_max_itr <- month_max
      } else {
        month_min_itr <- month_min
      }
    } else if (year == year_max) {
      month_max_itr <- month_max
    }
    
    my_dates_itr <- as.POSIXct(as.character(glue("{year}-{sprintf('%02d', month_min_itr:month_max_itr)}-01")))
    
    my_dates <- append(my_dates, format(my_dates_itr, "%b-%Y"))
  }
  
  return(my_dates)
}

#' Hack to deal with consolidating site codes in main GENIE>
#' 
#' @param site main GENIE site code
#' @return site codes corresponding to site code in current release.
get_site_list <- function(site) {
  if (site == "PROV") {
    return(c("PROV", "SCI"))
  }
  
  return(site)
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
  
  sites <- get_site_list(site)
  
  # merge and filter
  data <- patient_data %>% 
    inner_join(sample_data, by = "PATIENT_ID") %>%  
    filter(is.element(CENTER, sites)) %>%
    select(PATIENT_ID, 
           SAMPLE_ID, 
           ONCOTREE_CODE, 
           SEQ_DATE, 
           AGE_AT_SEQ_REPORT,
           SEQ_YEAR,
           YEAR_DEATH)
  
  return(data)
}

get_seq_dates <- function(config, phase, cohort, site) {
  
  seq_dates <- c()
  
  if (!is.null(config$phase[[phase]]$cohort[[cohort]]$site[[site]]$date)) {
    seq_dates <- config$phase[[phase]]$cohort[[cohort]]$site[[site]]$date
  } else {
    seq_dates <- config$phase[[phase]]$cohort[[cohort]]$date
  }
  
  return(seq_dates)
}

get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA,
                                           sep = ",", 
                                           na.strings = c("NA"), 
                                           header = T,
                                           check_names = F,
                                           comment.char = "#",
                                           colClasses = "character") {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = check_names,
                   header = header, comment.char = comment.char)
  return(data)
}

get_patient_ids_bpc_removed <- function(synid_table_patient_removal, cohort) {
  query <- glue("SELECT record_id FROM {synid_table_patient_removal} WHERE {cohort} = 'true'")
  res <- as.character(unlist(as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))))
  return(res)
}

get_sample_ids_bpc_removed <- function(synid_table_sample_removal, cohort) {
  query <- glue("SELECT SAMPLE_ID FROM {synid_table_sample_removal} WHERE {cohort} = 'true'")
  res <- as.character(unlist(as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))))
  return(res)
}

get_patient_ids_in_release <- function(synid_file_release) {
  
  data <- get_synapse_entity_data_in_csv(synid_file_release)
  return(unlist(data$record_id))
}

get_sample_ids_bpc_removed <- function(synid_table_sample_removal, cohort) {
  query <- glue("SELECT SAMPLE_ID FROM {synid_table_sample_removal} WHERE {cohort} = 'true'")
  res <- as.character(unlist(as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))))
  return(res)
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

get_number_eligible <- function(x) {
  
  mod <- x
  
  col_flags <- grep(pattern = "^FLAG_", x = colnames(x), value = T, invert = F)
  mod$flag_eligible <- apply(x[,col_flags], 1, all)
  
  # determine eligible samples (all flags TRUE)
  eligible <- as.data.frame(mod %>%
                              filter(flag_eligible) %>% 
                              group_by(PATIENT_ID) %>%
                              summarize(SAMPLE_IDS = paste0(SAMPLE_ID, collapse = ";")) %>%
                              select(PATIENT_ID, SAMPLE_IDS))
  
  return(nrow(eligible))
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
#' @return Synapse ID of entity representing file
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
  
  return(file$properties$id)
}

# synapse login --------------------

status <- synLogin()

# read ----------------------------

# configuration files
status <- download.file(url, destfile = file_config, method = "wget")
config <- read_yaml(file_config)
sites <- names(config$phase[[phase]]$cohort[[cohort]]$site)

# date ranges
min_seq_date <- get_min_seq_date(synid_table_sample = config$synapse$main_sample$id, posix = T)
max_seq_date <- get_max_seq_date(synid_table_sample = config$synapse$main_sample$id, posix = T)

# main ----------------------------

# storage
exclude_patient_id <- c()
exclude_sample_id <- c()
n_eligible <- c()

# get vector of months
seq_min_dates_all <- get_month_dates(seq_min_date = min_seq_date, seq_max_date = max_seq_date)
n_eligible <- matrix(NA, nrow = length(seq_min_dates_all), ncol = length(sites), dimnames = list(seq_min_dates_all, sites))

# get number of eligible cases for each min seq date
for (site in sites) {
  
  if (verbose) {
    print(glue("site: {site}"))
  }
  
  # get all eligibility data for the cohort
  eligibility_data <- get_eligibility_data(synid_table_patient = config$synapse$main_patient$id, 
                                           synid_table_sample = config$synapse$main_sample$id, 
                                           site = site)
  
  # get IDs to exclude
  flag_prev_release <- (config$release$cohort[[cohort]]$patient_level_dataset != "NA")
  if (phase == 2 && flag_prev_release) {
    exclude_patient_id <- get_patient_ids_in_release(synid_file_release = config$release$cohort[[cohort]]$patient_level_dataset)
    exclude_patient_id <- append(exclude_patient_id, 
                                 get_patient_ids_bpc_removed(synid_table_patient_removal = config$synapse$bpc_removal_patient$id, 
                                                             cohort = cohort))
    exclude_sample_id <- get_sample_ids_bpc_removed(synid_table_sample_removal = config$synapse$bpc_removal_sample$id, 
                                                    cohort = cohort)
  }
  
  seq_dates <- get_seq_dates(config, phase, cohort, site)
  seq_min_dates <- intersect(seq_min_dates_all, get_month_dates(seq_min_date = min_seq_date, seq_max_date = seq_date_to_posix(seq_dates$seq_max)))
  
  for (seq_min in seq_min_dates) {
    
    if (verbose) {
      print(glue("seq_min: {seq_min}"))
    }
    
    eligibility_matrix <- create_eligibility_matrix(data = eligibility_data, 
                                                    allowed_codes = config$phase[[phase]]$cohort[[cohort]]$oncotree$allowed_codes, 
                                                    seq_min = seq_min, 
                                                    seq_max = seq_dates$seq_max,
                                                    exclude_patient_id = exclude_patient_id,
                                                    exclude_sample_id = exclude_sample_id)
    n_eligible[seq_min, site] <- get_number_eligible(x = eligibility_matrix) 
  }
}

# write 
write.csv(n_eligible, file = outfile)
if (!is.na(synid_folder_output)) {
  synid_file_output <- save_to_synapse(path = outfile, 
                              parent_id = synid_folder_output, 
                              prov_name = "case selection sweep", 
                              prov_desc = glue("case selection sweep for phase 2 {cohort} BPC cohort"), 
                              prov_used = c(config$synapse$main_patient$id, config$synapse$main_sample$id), 
                              prov_exec = "https://github.com/Sage-Bionetworks/genie-project-requests/blob/main/2022-05-20_shawn_case_selection_sweeps.R")
  
  file.remove(outfile)
}

# close out ----------------------------

# clean up
file.remove(file_config)

if (verbose) {
  if (!is.na(synid_folder_output)) {
    print(glue("file {outfile} saved to synapse at {synid_file_output}"))
  } else {
    print(glue("file written locally to {outfile}"))
  }
}

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
