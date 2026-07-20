# Description: Validate new drug masking reports.  
# Author: Haley Hunter-Zinck
# Date: September 2, 2021

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(synapser)
synLogin()

# synapse

# parameters
cohorts <- c("BrCa", "CRC")
sites <- c("DFCI", "MSK", "VICC")
date = "2021-07-16"

# constants
MAP_COHORT <- setNames(c("Breast", "CRC", "NSCLC", "Pancreas"), 
                       c("BrCa", "CRC", "NSCLC", "PANC"))

# functions ----------------------------

trim <- function(str) {
  front <- gsub(pattern = "^[[:space:]]+", replacement = "", x = str)
  back <- gsub(pattern = "[[:space:]]+$", replacement = "", x = front)
  
  return(back)
}

get_synapse_folder_children <- function(synapse_id) {
  
  
  ent <- as.list(synGetChildren(synapse_id))
  
  children <- c()
  for (i in 1:length(ent)) {
    children[ent[[i]]$name] <- ent[[i]]$id
  }
  
  return(children)
}

get_report_synapse_id <- function(cohort, site, date, synid_folder_qc = "syn21652594") {
  
  # get cohort subfolder synapse id
  synid_children_cohort <- get_synapse_folder_children(synid_folder_qc)
  synid_folder_cohort <- as.character(synid_children_cohort[MAP_COHORT[cohort]])
  
  # get synapse ID of folder labeled "qa_drug_review_qa_manager_signoff"
  synid_children_review <- get_synapse_folder_children(synid_folder_cohort)
  synid_folder_review <- as.character(synid_children_review["qa_drug_review"])
  
  # get synapse ID of folder for most recent previous review
  synid_children_date <- get_synapse_folder_children(synid_folder_review)
  synid_folder_date <- as.character(synid_children_date[glue("{date} Reports")])
  
  synid_children_report <- get_synapse_folder_children(synid_folder_date)
  synid_file_report <- as.character(synid_children_report[glue("unmasked_drugs_review_{site}_{cohort}_{date}.csv")])
  
  return(synid_file_report)
}

get_fda_approval_year <- function(bpc_short_name, synid_map = "syn26125434", synid_concept = "syn26119153", synid_rel = "syn26119155") {
  
  query <- glue("SELECT HemOnc_code FROM {synid_map} 
                WHERE BPC  LIKE '{bpc_short_name}%'
                  AND HemOnc_code NOT IN ('Other','NA')")
  drug_code <- as.double(as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))[1,])
  
  if (all(is.na(drug_code))) {
    return("no drug code")
  }
  
  query <- glue("SELECT concept_id_2 FROM {synid_rel} WHERE concept_id_1 = {drug_code} AND relationship_name = 'Was FDA approved yr'")
  year_code <- as.double(unlist(as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))))
  
  if (all(is.na(year_code))) {
    return("no fda approval year relationship code")
  }
  
  year_code_list <- paste0("'", paste0(year_code, collapse = "','"), "'")
  query <- glue("SELECT MAX(concept_name) FROM {synid_concept} WHERE concept_id IN ({year_code_list}) ")
  year_fda <- as.double(unlist(as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))))
  
  if (all(is.na(year_fda))) {
    return("fda year concept name is NA")
  }
  
  return(year_fda)
}

# read ----------------------------

datas <- list()
synid_reports <- c()

for (cohort in cohorts) {
  
  for (site in sites) {
    label <- glue("{cohort}_{site}")
    synid_reports[label] <- get_report_synapse_id(cohort = cohort, site = site, date = date)
    datas[[label]] <- read.csv(synGet(as.character(synid_reports[label]))$path, stringsAsFactors = F)
  }
}

# check fda status ----------------------------

# check clinical trial (No) and fda status (unapproved)
for (i in seq_len(length(datas))) {
  
  idx <- which(datas[[i]][,"drugs_ct_yn"] == "No" & datas[[i]][,"fda_status"] == "unapproved")
  
  for (j in idx) {
    drugs <- trim(strsplit(datas[[i]][j, "drugs_in_regimen"], split = ',')[[1]])
    
    years <- c()
    for (drug in drugs) {
      years[drug] <- get_fda_approval_year(drug)
    }
    
    print("--------")
    print(names(datas)[i])
    print(datas[[i]][j, "drugs_in_regimen"])
    print(cbind(drugs, years))
    print("--------")
  }
}

# > 89 ----------------------------

for (i in seq_len(length(datas))) {
  
  idx_89 <- which(datas[[i]][,"age_greater_89"] == "TRUE")
  cat(glue("{names(datas)[i]} > 89: {length(idx_89)}"), "\n")
}

# investigational duration ----------------------------

for (i in seq_len(length(datas))) {
  
  idx_duration <- which(datas[[i]][,"investigational_duration"] != "ok")
  cat(glue("{names(datas)[i]} invesigation duration not ok: {length(idx_duration)}"), "\n")
}

# investigational other ----------------------------

for (i in seq_len(length(datas))) {
  
  idx_other <- which(datas[[i]][,"investigational_other"] != "ok")
  cat(glue("{names(datas)[i]} invesigation other not ok: {length(idx_other)}"), "\n")
}


# previous sign off  ----------------------------

for (i in seq_len(length(datas))) {
  
  idx <- which(datas[[i]][,"previous_signoff"] != datas[[i]][,"qa_manager_signoff"])
  cat(glue("{names(datas)[i]} previous and current different: {length(idx)}"), "\n")
}

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
