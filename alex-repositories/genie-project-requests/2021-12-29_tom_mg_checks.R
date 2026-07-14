# Description: Additional functions for main GENIE validator to front-load 
#               checks currently caught by the post-processing dashboard. 
# Implemented check functions:
# - check_assay_yaml_validity
# - check_seq_assay_id
# - check_pat_sam_id
# - check_contact_info
# - check_death_info
# Author: Haley Hunter-Zinck
# Date: 2021-12-29

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
  make_option(c("-i", "--synid_project"), type = "character", default = "syn3380222",
              help="Synapse ID of project folder (default: syn3380222)"),
  make_option(c("-c", "--center"), type = "character", default = "Cancer Research UK",
              help="Center abbreviation (default: 'Cancer Research UK')"),
  make_option(c("-o", "--synid_folder_output"), type = "character", default = NULL,
              help="Synapse ID of output folder (optional)"),
  make_option(c("-v", "--verbose"), action="store_true", default = FALSE, 
              help="Output script messages to the user (default = FALSE).")
)
opt <- parse_args(OptionParser(option_list=option_list))

synid_project <- opt$synid_project
center <- opt$center
synid_folder_output <- opt$synid_folder_output
verbose <- opt$verbose

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(yaml)
library(synapser)
synLogin()

# print parameters
if (verbose) {
  print(glue("Parameters"))
  print(glue("- Project: '{synGet(synid_project, downloadFile = F)$properties$name}' ({synid_project})"))
  if (is.null(synid_folder_output)) {
    print(glue("- Output folder: {getwd()}"))
  } else {
    print(glue("- Output folder: '{synGet(synid_folder_output, downloadFile = F)$properties$name}' {synid_folder_output}"))
  }
  print(glue("- Center: '{center}'"))
  print(glue("- Verbose: {verbose}"))
  print(glue("--------"))
}

# generic functions ----------------------------

#' Download and load data stored in csv or other delimited format on Synapse
#' into an R data frame.
#' 
#' @param synapse_id Synapse ID
#' @version Version of the Synapse entity to download.  NA will load current
#' version
#' @param set Delimiter for file
#' @param na.strings Vector of strings to be read in as NA values
#' @param header TRUE if the file contains a header row; FALSE otherwise.
#' @param check_names TRUE if column names should be modified for compatibility 
#' with R upon reading; FALSE otherwise.
#' @return data frame
get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA,
                                           sep = ",", 
                                           na.strings = c("NA"), 
                                           header = T,
                                           check_names = F,
                                           comment.char = "#") {
  
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

#' Get all child entities of a synapse folder.
#' 
#' @param synapse_id Synapse ID of the folder
#' @param include_types Types of child entities to return
#' @return Vector with values as Synapse IDs and names as entity names.
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

#' Depth first search for tree traversal of root synapse ID
#' of Synapse folder or project.
#' 
#' @param synid_root Synapse ID of the root folder/project.
#' @return Vector of Synapse IDs of all descendants of the root 
#' @example 
#' traverse_synapse("syn12345")
#' Depth first search for tree traversal of root synapse ID
#' of Synapse folder or project.
#' 
#' @param synid_root Synapse ID of the root folder/project.
#' @param exclude Pattern match of entity names to exclude from the traverse.
#' @return Vector of Synapse IDs of all descendants of the root 
#' @example 
#' traverse_synapse("syn12345")
traverse_synapse <- function(synid_root, exclude = NULL) {
  
  synid_folders <- c()
  synid_children <- as.list(synGetChildren(synid_root, 
                                           includeTypes = list("folder", "file")))
  
  if(!length(synid_children)) {
    return(synid_root)
  }
  
  for (synid_child in synid_children) {
    if (is.null(exclude) || !grepl(synid_child$name, pattern = exclude, ignore.case = T)) {
      synid_folders <- append(synid_folders, traverse_synapse(synid_child$id, exclude = exclude))
    }
  }
  
  return(c(synid_root, synid_folders))
}

#' Get the name of a Synapse entity. 
#' 
#' @param synapse_id Synapse ID string
#' @return String representing entity name
#' @example get_synapse_entity_name("syn12345")
get_synapse_entity_name <- function(synapse_id) {
  return(synGet(synapse_id, downloadFile = F)$properties$name)
}

#' Get the name of a Synapse entities. 
#' 
#' @param synapse_ids Vector of Synapse IDs
#' @return Vector of entity names
#' @example get_synapse_entity_name(c("syn54321", syn12345"))
get_synapse_entity_names <- function(synapse_ids) {
  
  synapse_names <- c()
  
  for (synapse_id in synapse_ids) {
    synapse_names <- append(synapse_names, get_synapse_entity_name(synapse_id))
  }
  return(synapse_names)
}

# helper functions for main genie ----------------------

#' Get Synapse ID of center's input files.  
#' 
#' @param synid_project Synapse ID of relevant project
#' @param center Name of center corresponding to umbrella folder
#' @return Synapse ID
get_synid_folder_center <- function(synid_project, center) {
  path_folder <- glue("Centers/{center}/Input")
  synid_folder <- get_folder_synid_from_path(synid_folder_root = synid_project, 
                             path = path_folder)
  
  return(synid_folder)
}

#' Get a specific main genie file by descriptor.  
#' 
#' @param synid_folder_data Synapse ID of folder containing all relevant files
#' @param file_type Type of file to obtain
#' @return Synapse ID
get_synid_file_mg <- function(synid_folder_data, 
                                file_type = c("assay", "sample", "clinical"),
                                exclude = "vcf") {
  
  file_name <- NA
  synid_folder_children <- traverse_synapse(synid_root = synid_folder_data, exclude = exclude)
  synid_folder_children <- setNames(synid_folder_children, get_synapse_entity_names(synid_folder_children))
  
  if (file_type == "assay") {
    return(as.character(synid_folder_children["assay_information.yaml"]))
  }
  
  if (file_type == "sample" || file_type == "patient") {
    file_name <- grep(pattern = "data_clinical_supp", x = names(synid_folder_children), value = )
    
    if (length(file_name) == 1) {
      return(as.character(synid_folder_children[file_name]))
    }
    
    if (length(file_name) > 1 ) {
        file_name <- grep(pattern = glue("data_clinical_supp_{file_type}"), x = names(synid_folder_children), value = )
        return(as.character(synid_folder_children[file_name]))
    }
  }
  
  return(file_name)
}

#' Format column names of clinical files for standard processing.
#' - convert to all upper case
#' - replace any duplicate column names with unique column name
#' 
#' @param raw Vector of strings representing raw column names
#' @return Vector of strings
clean_column_names <- function(raw) {
  mod <- toupper(raw)
  mod[which(duplicated(raw))] <- glue("{mod[which(duplicated(raw))]}_2")
  return(mod)
}

#' Format sequencing assay IDs for standard processing.
#' - convert to all lower case
#' - replace any underscores with hyphens
#' 
#' @param raw Vector of strings representing raw column names
#' @return Vector of strings
clean_seq_ids <- function(raw) {
  mod <- tolower(raw)
  mod <- gsub(pattern = "_", replacement = "-", x = mod)
  return(mod)
}

# check functions for main genie -----------------------

check_assay_yaml_validity_strict <- function(synid_file_yaml) {
  
  data <- tryCatch({
    data_seq <- read_yaml(synGet(synid_file_yaml)$path)
  }, error = function(cond) {
    error_invalid <- T
    return(NULL)
  }, warning = function(cond) {
    return(NULL)
  })
  
  return(!is.null(data))
}

check_assay_yaml_validity_loose <- function(synid_file_yaml) {
  
  data <- tryCatch({
    data_seq <- read_yaml(synGet(synid_file_yaml)$path)
  }, error = function(cond) {
    error_invalid <- T
    return(NULL)
  })
  
  return(!is.null(data))
}


#' Check that assay_information.yaml has a valid format.
#' 
#' @param synid_project Synapse ID of project
#' @param center Name of center corresponding to center folder
#' @param warning_invalid Consider YAML files generating warnings in addition to any errors as invalid;
#' otherwise, only errors indicate invalid
#' @return TRUE if valid, otherwise FALSE
check_assay_yaml_validity <- function(synid_project, center, synid_file_seq = NULL, strict = T) {
  
  if (is.null(synid_file_seq)) {
    synid_folder_data <- get_synid_folder_center(synid_project, center)
    synid_file_seq <- get_synid_file_mg(synid_folder_data = synid_folder_data, 
                                        file_type = "assay")
  }
  
  res <- NA
  if (strict) {
    res <- check_assay_yaml_validity_strict(synid_file_seq)
  } else {
    res <- check_assay_yaml_validity_loose(synid_file_seq)
  }
  
  return(res)
}

#' Check for sequencing assay IDs listed in clinical file but not in the metadata file.
#' 
#' @param synid_project Synapse ID of project
#' @param center Name of center corresponding to center folder
#' @param fuzzy boolean indicating whether fuzzy matching should be used for comparing seq_assay_ids;
#' otherwise, exact match used
#' @return Vector of seq_assay_id values in sample file but not metadata or NULL if none found
check_seq_assay_id <- function(synid_project, center, synid_file_seq = NULL, synid_file_sam = NULL, fuzzy = F) {
  
  if (is.null(synid_file_seq)) {
    synid_folder_data <- get_synid_folder_center(synid_project, center)
    synid_file_seq <- get_synid_file_mg(synid_folder_data = synid_folder_data, 
                                        file_type = "assay")
  }
  if (is.null(synid_file_sam)) {
    synid_folder_data <- get_synid_folder_center(synid_project, center)
    synid_file_sam <- get_synid_file_mg(synid_folder_data = synid_folder_data, 
                                        file_type = "sample")
  }
  
  data_seq <- read_yaml(synGet(synid_file_seq)$path)
  data_sam <- get_synapse_entity_data_in_csv(synid_file_sam, sep = "\t")
  colnames(data_sam) <- clean_column_names(colnames(data_sam))
  
  assay_seq <- unlist(lapply(data_seq, function(x) {return(x$assay_specific_info[[1]]$SEQ_ASSAY_ID)}))
  assay_sam_raw <- unlist(data_sam %>% select(SEQ_ASSAY_ID) %>% distinct())
  assay_sam <- assay_sam_raw
  if (fuzzy) {
    assay_seq <- clean_seq_ids(assay_seq)
    assay_sam <- clean_seq_ids(assay_sam)
  }

  res <- assay_sam_raw[match(setdiff(assay_sam, assay_seq), assay_sam)]
  if (length(res)) {
    return(res)
  }
  return(NULL)
}

#' List any patient IDs in the sample file but not in the patient file,
#' if the files are uploaded as separate files  
#' 
#' @param synid_project Synapse ID of project
#' @param center Name of center corresponding to center folder
#' @return Vector of patient IDs in sample but not patient file or NULL if none found
check_pat_sam_id <- function(synid_project, center, synid_file_pat = NULL, synid_file_sam = NULL) {
  
  if (is.null(synid_file_pat)) {
    synid_folder_data <- get_synid_folder_center(synid_project, center)
    synid_file_pat <- get_synid_file_mg(synid_folder_data = synid_folder_data, 
                                        file_type = "patient")
  }
  if (is.null(synid_file_sam)) {
    synid_folder_data <- get_synid_folder_center(synid_project, center)
    synid_file_sam <- get_synid_file_mg(synid_folder_data = synid_folder_data, 
                                        file_type = "sample")
  }
  
  if (synid_file_pat == synid_file_sam) {
    return(NULL)
  }
  
  data_pat <- get_synapse_entity_data_in_csv(synid_file_pat, sep = "\t")
  data_sam <- get_synapse_entity_data_in_csv(synid_file_sam, sep = "\t")
  colnames(data_pat) <- clean_column_names(colnames(data_pat))
  colnames(data_sam) <- clean_column_names(colnames(data_sam))
  
  id_pat <- unlist(data_pat %>% select(PATIENT_ID) %>% distinct())
  id_sam <- unlist(data_sam %>% select(PATIENT_ID) %>% distinct())
  
  res <- setdiff(id_sam, id_pat)
  if (length(res)) {
    return(res)
  }
  return(NULL)
}

#' Check for patient contact information inconsistencies.
#' Specifically, check whether the contact information is missing for the 
#' year but not the interval or vice versa for a given patient.
#' 
#' @param synid_project Synapse ID of project
#' @param center Name of center corresponding to center folder
#' @return Vector of patient IDs with inconsistent contact info or NULL
check_contact_info <- function(synid_project, center, synid_file_pat = NULL,
                              missing = c("Not Collected", "Unknown", "Not Applicable", NA)) {
  
  if (is.null(synid_file_pat)) {
    synid_folder_data <- get_synid_folder_center(synid_project, center)
    synid_file_pat <- get_synid_file_mg(synid_folder_data = synid_folder_data, 
                                        file_type = "patient")
  }
  
  data_pat <- get_synapse_entity_data_in_csv(synid_file_pat, sep = "\t", na.strings = c("NA", ""))
  colnames(data_pat) <- clean_column_names(colnames(data_pat))
  
  if (length(setdiff(c("YEAR_CONTACT", "INT_CONTACT"), colnames(data_pat)))) {
    return(NULL)
  }
  
  results <- unlist(data_pat %>% 
    filter(is.element(YEAR_CONTACT, missing) & !is.element(INT_CONTACT, missing) |
             !is.element(YEAR_CONTACT, missing) & is.element(INT_CONTACT, missing)) %>% 
    select(PATIENT_ID))
  
  if (length(results)) {
    return(results)
  }
  return(NULL)
}

#' Check for patient death data inconsistencies.
#' Specifically, check whether the death information is missing 
#' for the year but not the interval or vice versa for a given patient.
#' 
#' @param synid_project Synapse ID of project
#' @param center Name of center corresponding to center folder
#' @return Vector of patient IDs with inconsistent contact info or NULL
check_death_info <- function(synid_project, center, synid_file_pat = NULL,
                             missing = c("Not Collected", "Unknown", "Not Applicable", NA)) {
  
  if (is.null(synid_file_pat)) {
    synid_folder_data <- get_synid_folder_center(synid_project, center)
    synid_file_pat <- get_synid_file_mg(synid_folder_data = synid_folder_data, 
                                        file_type = "patient")
  }
  
  data_pat <- get_synapse_entity_data_in_csv(synid_file_pat, sep = "\t", na.strings = c("NA", ""))
  colnames(data_pat) <- clean_column_names(colnames(data_pat))
  
  if (length(setdiff(c("YEAR_DEATH", "INT_DOD", "DEAD"), colnames(data_pat)))) {
    return(NULL)
  }
  
  results <- unlist(data_pat %>% 
    filter(is.element(YEAR_DEATH, missing) & !is.element(INT_DOD, missing) |
             !is.element(YEAR_DEATH, missing) & is.element(INT_DOD, missing) |
             ((!is.element(YEAR_DEATH, missing) | !is.element(INT_DOD, missing)) & !as.logical(DEAD))) %>% 
    select(PATIENT_ID))
  
  if (length(results)) {
    return(results)
  }
  return(NULL)
}

# main ----------------------------

res_all <- matrix(NA, nrow = 0, ncol = 3, dimnames = list(c(), c("issue_no","flagged_value", "description")))

synid_folder_data <- get_synid_folder_center(synid_project, center)
synid_file_seq <- get_synid_file_mg(synid_folder_data = synid_folder_data, 
                                    file_type = "assay")
synid_file_pat <- get_synid_file_mg(synid_folder_data = synid_folder_data, 
                                    file_type = "patient")
synid_file_sam <- get_synid_file_mg(synid_folder_data = synid_folder_data, 
                                    file_type = "sample")

res_yaml_error <- check_assay_yaml_validity(synid_project, center, synid_file_seq = synid_file_seq, strict = F)
res_yaml_warn <- check_assay_yaml_validity(synid_project, center, synid_file_seq = synid_file_seq, strict = T)
if (verbose) {
  print(glue("assay_information.yaml valid (no errors): {res_yaml_error}"))
  print(glue("assay_information.yaml valid (no errors or warnings): {res_yaml_warn}"))
}

if (res_yaml_error) {
  res_seq <- check_seq_assay_id(synid_project, center, synid_file_seq = synid_file_seq, 
                                synid_file_sam = synid_file_sam, fuzzy = T)
  
  if (verbose) {
    print(glue("number of seq_assay_ids absent form metadata: {length(res_seq)}"))
  }
} else {
  res_seq <- NULL
  
  if (verbose) {
    print(glue("yaml file invalid so not checking seq_assay_id metadata"))
  }
}

res_pat <- check_pat_sam_id(synid_project, center, synid_file_pat = synid_file_pat, synid_file_sam = synid_file_sam)
if (verbose) {
  print(glue("number of patient ids in sample not patient file: {length(res_pat)}"))
}

res_contact <- check_contact_info(synid_project, center, synid_file_pat = synid_file_pat)
if (verbose) {
  print(glue("number of patients with inconsistent contact info: {length(res_contact)}"))
}

res_death <- check_death_info(synid_project, center, synid_file_pat = synid_file_pat)
if (verbose) {
  print(glue("number of patients with inconsistent contact info: {length(res_death)}"))
}

if (!res_yaml_error) {
  res_all <- rbind(res_all, cbind(NA, synid_file_seq, "yaml generates errors"))
}
if (res_yaml_error && !res_yaml_warn) {
  res_all <- rbind(res_all, cbind(NA, synid_file_seq, "yaml generates warnings"))
}
if (!is.null(res_seq)) {
  res_all <- rbind(res_all, cbind(NA, res_seq, "seq_assay_id in sample file but not assay_information.yaml"))
}
if (!is.null(res_pat)) {
  res_all <- rbind(res_all, cbind(NA, res_pat, "patient IDs in sample but not patient file"))
}
if (!is.null(res_contact)) {
  res_all <- rbind(res_all, cbind(NA, res_contact, "contact year and interval are inconsistently missing"))
}
if (!is.null(res_death)) {
  res_all <- rbind(res_all, cbind(NA, res_death, "death year or interval or status are inconsistent"))
}

res_all[,"issue_no"] <- c(1:nrow(res_all))

# write --------------------------------

Sys.setenv(TZ='America/Los_Angeles')
file_output <- glue("{Sys.Date()}_{tolower(gsub(center, pattern = ' ', replacement = '_'))}.csv")
write.csv(res_all, file = file_output, row.names = F)

if (!is.null(synid_folder_output)) {
  synid_file_output = save_to_synapse(path = file_output, 
                                      parent_id = synid_folder_output, 
                                      prov_name = "extra main genie checks", 
                                      prov_desc = "additional checks to perform on input files for main genie", 
                                      prov_used = c(synid_file_seq, synid_file_sam, synid_file_pat), 
                                      prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2021-12-29_tom_mg_checks.R")
  
  file.remove(file_output)
}

# close out ----------------------------

if (verbose) {
  
  print(glue("--------"))
  print(glue("total number of issues flagged: {nrow(res_all)}"))
  
  if (is.null(synid_folder_output)) {
    print(glue("Results written locally as '{file_output}'."))
  } else {
    print(glue("Results uploaded to Synapse as '{synGet(synid_file_output, downloadFile = F)$properties$name}' (synid_file_output)."))
  }
}

if (verbose) {
  toc = as.double(Sys.time())
  print(glue("Runtime: {round(toc - tic)} s"))
}
