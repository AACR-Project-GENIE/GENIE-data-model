# Description: explore whether we can replace RCC functionality with the prostate cohort
# Author: Haley Hunter-Zinck
# Date: September 13, 2021

# user input ----------------------------

# args <- commandArgs(trailingOnly = F)
# if (length(args) != 1) {
#   print("Usage: R -f script.R --args <cohort>")
# }
# cohort <- args[1]

# debug
cohort <- "BrCa"

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(synapser)
synLogin()
library(yaml)
library(dplyr)
library(lubridate)

# synapse
synid_file_prostate_rcc_all <- "syn25610356"

# configuration
config <- read_yaml("config.yaml")
debug <- config$misc$debug

# functions ----------------------------

now <- function(timeOnly = F, tz = "US/Pacific") {
  
  Sys.setenv(TZ=tz)
  
  if(timeOnly) {
    return(format(Sys.time(), "%H:%M:%S"))
  }
  
  return(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
}

#' Return a file upload data corresponding to a cohort-site pair as an R object.
#'
#' @param cohort cohort of interest
#' @param site site of interest
#' @param obj upload file object from config
#' @return Contents of a cohort-side data upload file as an R data frame.
#' @example
#' get_bpc_data_upload(cohort, site, list(data1 = "syn12345", data2 = "syn6554332", 
#'                           header1 = "syn39857289375, header2 = "syn11111"),
#' get_bpc_data_upload(cohort, site, list(data1 = "syn12345"))
get_bpc_data_upload <- function(cohort, site, obj) {
  
  data <- c()
  data1 <- c()
  data2 <- c()
  
  # get data 1 (default, should always be specified)
  ent <- synGet(obj$data1)
  data1 <- read.csv(ent$path, 
                    check.names = F,
                    na.strings = c(""), 
                    stringsAsFactors = F)
  
  # check for header1
  if (length(obj$header1)) {
    ent <- synGet(obj$header1)
    colnames(data1) <- as.character(read.csv(ent$path, check.names = F,
                                             na.strings = c(""),
                                             stringsAsFactors = F))
  }
  
  # check for data 2
  if (length(obj$data2)) {
    ent <- synGet(obj$data2)
    data2 <- read.csv(ent$path, check.names = F,
                      na.strings = c(""), 
                      stringsAsFactors = F)
  }
  
  # check for header2
  if (length(obj$header2)) {
    ent <- synGet(obj$header2)
    colnames(data2) <- as.character(read.csv(ent$path, check.names = F,
                                             na.strings = c(""),
                                             stringsAsFactors = F))
  }
  
  if (length(obj$data2)) {
    data <- data1 %>% inner_join(data2, by = c("record_id", 
                                               "redcap_repeat_instrument", 
                                               "redcap_repeat_instance"))
  } else {
    data <- data1
  }
  
  return(data)
}

#' Get Synapse IDs of the most recent PRISSMM documentation files
#' for a cohort.
#'
#' @param synid_table_prissmm Synapse ID of the table containing Synapse IDs
#' of all PRISSM documentation
#' @param cohort cohort of interest
#' @file_name name of the type of documentation file of interest
#' @return Synapse ID of the most recent PRISSMM documentation corresponding
#' to the cohort and type of document.
#' @example
#' get_bpc_synid_prissmm("syn12345", "my_cohort", "Import Template")
get_bpc_synid_prissmm <- function(synid_table_prissmm, cohort,
                                  file_name = c("Import Template", "Data Dictionary non-PHI")) {
  
  query <- glue("SELECT id FROM {synid_table_prissmm} \
                WHERE cohort = '{cohort}' \
                ORDER BY name DESC LIMIT 1")
  ent_folder <- synTableQuery(query, includeRowIdAndRowVersion = F)
  synid_prissmm_children <- as.list(synGetChildren(as.character(
    read.csv(ent_folder$filepath, stringsAsFactors = F))))
  synid_prissmm_children_ids <- setNames(unlist(lapply(synid_prissmm_children,
                                                       function(x) {return(x$id)})),
                                         unlist(lapply(synid_prissmm_children,
                                                       function(x) {return(x$name)})))
  ent <- synGet(as.character(synid_prissmm_children_ids[file_name]))
  synid_file_template <- ent$get("id")
  
  return(synid_file_template)
}

#' Merge several data frames that may have different columns.
#' 
#' @param datasets List of data frames
#' @return Single data frame
#' @example 
#' merge_datasets(datasets = list(a = data_a, b = data_b, c = data_c))
merge_datasets <- function(datasets) {
  
  mod <- list()
  
  for (i in 1:length(datasets)) {
    mod[[i]] <- data.frame(lapply(get_bpc_data_upload(cohort = cohort, 
                                          site = site, 
                                          obj = config_cohort[[i]]), as.character), 
                           stringsAsFactors = F)
  }
  
  merged <- bind_rows(mod)
  return(merged)
}

get_root_variable_name <- function(str) {
  splt <- strsplit(x = str, split = "___")
  root <- unlist(lapply(splt, FUN = function(x) {return(x[1])}))
  return(root)
}

get_site_from_record_id <- function(x) {
  site <- unlist(lapply(strsplit(x = x, split = "-"), FUN = function(x) {return(x[2])}))
  return(site)
}

trim_string <- function(str) {
  front <- gsub(pattern = "^[[:space:]]+", replacement = "", x = str)
  back <- gsub(pattern = "[[:space:]]+$", replacement = "", x = front)
  
  return(back)
}

merge_last_elements <- function(x, delim) {
  
  y <- c()
  y[1] = x[1]
  y[2] <- paste0(x[2:length(x)], collapse = delim)
  return(y)
}

#' Perform string split operation but only on the first
#' occurrence of the split character.
strsplit_first <- function(x, split) {
  
  unmerged <- strsplit(x = x, split = split)
  remerge <- lapply(unmerged, merge_last_elements, delim = split)
  
  return(remerge)
}

parse_mapping <- function(str) {
  
  clean <- trim_string(gsub(pattern = "\"", replacement = "", x = str))
  splt <- strsplit_first(strsplit(x = clean, split = "|", fixed = T)[[1]], split = ",")
  
  codes <- unlist(lapply(splt, FUN = function(x) {return(trim_string(x[1]))}))
  values <- unlist(lapply(splt, FUN = function(x) {return(trim_string(x[2]))}))
  mapping <- data.frame(cbind(codes, values), stringsAsFactors = F)
  
  return(mapping)
}

parse_mappings <- function(strs, labels) {
  
  mappings <- list()
  
  for (i in 1:length(strs)) {
    
    if (!is.na(strs[[i]])) {
      mappings[[labels[i]]] <- parse_mapping(strs[i])
    } else {
      mappings[[labels[i]]] <- NULL
    }
  }
  
  return(mappings)
}

uncode_code <- function(code, mapping) {
  
  idx <- which(mapping[,1] == code)
  
  if (length(idx)) {
    return(mapping[idx, 2])
  }
 
  return(code) 
}

uncode_column_names <- function(column_names, mappings) {
  
  uncoded_names <- column_names
  var_names <- get_root_variable_name(column_names)
  
  idx <- which(var_names != column_names) 
  for (i in 1:length(idx)) {
    
    mapping <- mappings[[var_names[idx[i]]]]
    coded_value <- strsplit(x = column_names[idx[i]], split = "___")[[1]][2]
    uncoded_value <- uncode_code(code = coded_value, mapping = mapping)
    uncoded_names[idx[i]] <- glue("{var_names[idx[i]]}___{uncoded_value}")
  }
  
  return(uncoded_names)
}

uncode_data_column <- function(col_coded, mapping) {
  
  # map coded to uncoded
  col_uncoded <- data.frame(codes = col_coded) %>%
    mutate(codes_chr = as.character(codes)) %>%
    left_join(mapping, by = c("codes_chr" = "codes")) %>%
    select("values")
  
  return(col_uncoded)
}

#' Map any coded data to actual values as mapped in the 
#' REDCap Data Dictionary (DD).
#' 
#' @param data Data frame of coded data
#' @param mappings Matrix with two columns, first containing a label and
#' second columns a mapping string.  
#' @param secondary_mappings Another mapping matrix that is used secondarily
#' if the label is not found in the primary mapping matrix.
#' @return Data frame of uncoded data.
#' @example
#' map_code_to_value(data = my_data, mappings = mappings_dd)
uncode_data <- function(df_coded, mappings, secondary_mappings = NA) {
  
  df_uncoded <- df_coded
  mapping_complete <- data.frame(codes = names(config$mapping$complete),
                                 values = as.character(config$mapping$complete),
                                 stringsAsFactors = F)

  for (i in 1:ncol(df_coded)) {
    
    var_name <- get_root_variable_name(names(df_coded)[i])
    
    if (length(which(names(mappings) == var_name))) {
      
      idx_mapping <- which(names(mappings) == var_name)
      
      if (var_name == names(df_coded)[i]) {
        df_uncoded[,i] <- uncode_data_column(col_coded = df_coded[,i], 
                                             mapping = mappings[[idx_mapping]])
      }
    } else if (length(which(names(secondary_mappings) == var_name))) {
      
      idx_mapping <- which(names(secondary_mappings) == var_name)
      
      if (var_name == names(df_coded)[i]) {
        df_uncoded[,i] <- uncode_data_column(col_coded = df_coded[,i], 
                                             mapping = secondary_mappings[[idx_mapping]])
      }
    } else if(grepl(pattern = "complete", x = var_name)) {
      df_uncoded[,i] <- uncode_data_column(col_coded = df_coded[,i], 
                                           mapping = mapping_complete)
    }
  }
  
  colnames(df_uncoded) <- uncode_column_names(column_names = colnames(df_uncoded), 
                                              mappings = mappings)
  
  return(df_uncoded)
}

convert_string_to_date <- function(x, format = "%Y-%m-%d %H:%M") {
  
  result <- x
  
  idx_slash <- which(grepl(x = x, pattern = "/"))
  if (length(idx_slash)) {
    result[idx_slash] <- format(mdy_hm(x[idx_slash]), format)
  }
  
  idx_19 <- which(nchar(x) == 19)
  if (length(idx_19)) {
    result[idx_19] <- format(ymd_hms(x[idx_19]), format)
  }
    
  idx_16 <- which(nchar(x) == 16 | nchar(x) == 15 & grepl(x = x, pattern = "-"))
  if (length(idx_16)) {
    result[idx_16] <- format(ymd_hm(x[idx_16]), format)
  }
  
  return(result)
}

format_rcc <- function(x, dd) {
  
  # modify date time format
  idx_dt <- grep(colnames(x), pattern = "time")
  #x[,idx_dt] <- lapply(x[,idx_dt], substring, first = 1, last = 16)
  x[,idx_dt] <- lapply(x[,idx_dt], convert_string_to_date)
  
  # modify boolean values values
  idx_ind <- dd$`Variable / Field Name`[which(dd$`Field Type` == "yesno")]
  x[,idx_ind] <- lapply(x[,idx_ind, drop = F], FUN = function(x) {return(tolower(as.logical(as.integer(x))))})
  
  # modify row values
  x$redcap_repeat_instrument[which(is.na(x$redcap_repeat_instrument))] = "no-repeat"

  # add missing columns
  site <- get_site_from_record_id(x$record_id)
  status <- rep("Enrolled", nrow(x))
  x = cbind(x, site, status)
  
  # modify column names
  colnames(x)[which(colnames(x) == "site")] <- "Site Name"
  colnames(x)[which(colnames(x) == "status")]<- "Subject Status"
  colnames(x)[which(colnames(x) == "record_id")] <- "Study Subject ID"
  colnames(x)[which(colnames(x) == "redcap_repeat_instrument")] <- "Event Name(Occurrence)"
  colnames(x)[which(colnames(x) == "redcap_repeat_instance")] <- "Instrument Occurrence"
  colnames(x)[which(colnames(x) == "redcap_ca_index")] <- "_redcap_ca_index"
  colnames(x)[which(colnames(x) == "redcap_ca_seq")] <- "_redcap_ca_seq"

  return(x)
}

save_to_synapse <- function(file_name, parent_id, prov_name = "", prov_desc = "", prov_used = "", prov_exec = "") {
  file <- File(path = file_name, parentId = parent_id)
  act <- Activity(name = prov_name,
                  description = prov_desc,
                  used = prov_used,
                  executed = prov_exec)
  file <- synStore(file, activity = act)
  
  return(T)
}

# read ----------------------------

if (debug) {
  print(glue("{now(timeOnly = T)}: Reading previous RCC export..."))
}

rcc <- read.csv(synGet(config$output[[cohort]]$id)$path,
                sep = "\t",
                skip = config$output[[cohort]]$skip,
                header = T,
                stringsAsFactors = F,
                check.names = F,
                na.strings = c(""))

if (debug) {
  print(glue("{now(timeOnly = T)}: Reading data dictionary..."))
}

synid_dd <- get_bpc_synid_prissmm(synid_table_prissmm = config$synapse$synid_table_prissmm, 
                                  cohort = cohort,
                                  file_name = "Data Dictionary non-PHI")
dd <- read.csv(synGet(synid_dd)$path, 
               sep = ",", 
               stringsAsFactors = F,
               check.names = F,
               na.strings = c(""))

if (debug) {
  print(glue("{now(timeOnly = T)}: Reading global response set..."))
}

grs <- read.csv(synGet(config$synapse$synid_file_grs)$path, 
                sep = ",", 
                stringsAsFactors = F,
                check.names = F,
                na.strings = c(""))

if (debug) {
  print(glue("{now(timeOnly = T)}: Reading data uploads..."))
}

data_upload <- list()
for (i in 1:length(config$upload[[cohort]])) {
  
  config_cohort <- config$upload[[cohort]]
  site <- names(config_cohort)[i]
  
  if (debug) {
    print(glue("  {now(timeOnly = T)}: Reading data upload for site {site}..."))
  }
  
  data_upload[[site]] <- get_bpc_data_upload(cohort = cohort, 
                                             site = site, 
                                             obj = config_cohort[[site]])
}

# main ----------------------------

if (debug) {
  print(glue("{now(timeOnly = T)}: Generating parsed mappings..."))
}

# mappings
mappings_dd <- parse_mappings(strs = dd[[config$column_name$variable_mapping]], 
                                           labels = dd[[config$column_name$variable_name]])
mappings_grs <- parse_mappings(strs = grs[,config$column_name$rs_value], 
                             labels = grs[,config$column_name$rs_label])

if (debug) {
  print(glue("{now(timeOnly = T)}: Merging data uploads..."))
}

# merge
coded <- merge_datasets(data_upload)

if (debug) {
  print(glue("{now(timeOnly = T)}: Uncoding data uploads..."))
}

# uncode
uncoded <- uncode_data(df_coded = coded, 
                       mappings = mappings_grs,
                       secondary_mappings = mappings_dd)

if (debug) {
  print(glue("{now(timeOnly = T)}: Formatting uncoded data like RCC..."))
}

# format like RCC
rcc_from_uncoded <- format_rcc(uncoded, dd = dd)

# later: write to file
file_output <- glue("{cohort}_RCC_export.tsv")
write.table(x = rcc_from_uncoded, file = file_output, row.names = F, sep = "\t")

# write to Synapse and clean up
# save_to_synapse(file_name = file_output, 
#                 parent_id = config$synapse$synid_folder_rcc_exports, 
#                 prov_name = "", 
#                 prov_desc = "", 
#                 prov_used = "", 
#                 prov_exec = "")
#file.remove(file_output)

# validate ----------------------------
dim(uncoded)
dim(rcc)
setdiff(uncoded$record_id, rcc$`Study Subject ID`)
setdiff(rcc$`Study Subject ID`, uncoded$record_id)
setdiff(colnames(rcc), colnames(uncoded))
setdiff(colnames(uncoded), colnames(rcc))

# after column formatting
setdiff(colnames(rcc), colnames(rcc_from_uncoded))
setdiff(colnames(rcc_from_uncoded), colnames(rcc))

# get rows that don't match
key_uncoded <- glue("{rcc_from_uncoded$`Study Subject ID`}_{rcc_from_uncoded$`Event Name(Occurrence)`}_{rcc_from_uncoded$`Instrument Occurrence`}")
key_rcc <- glue("{rcc$`Study Subject ID`}_{rcc$`Event Name(Occurrence)`}_{rcc$`Instrument Occurrence`}")
setdiff(key_rcc, key_uncoded)
setdiff(key_uncoded, key_rcc)
# none of the 3 keys in rcc but not the uncoded are in the med onc assessment --> empty rows removed

# same values for each column value
idx_matched <- match(key_rcc, key_uncoded)
idx_not_na <- which(!is.na(idx_matched))
col_remove <- setdiff(colnames(rcc), colnames(rcc_from_uncoded))
comp_val_result <- c()
comp_na_result <- c()
for (i in 1:ncol(rcc)) {
 if(!is.element(colnames(rcc)[i], col_remove)) {
   
   col_rcc <- rcc[idx_not_na,i]
   col_rcc_from_uncoded <- rcc_from_uncoded[idx_matched[idx_not_na],colnames(rcc)[i]]
   
   comp_val_result[i] <- all(col_rcc == col_rcc_from_uncoded, na.rm = T)
   comp_na_result[i] <- length(which(is.na(col_rcc))) == length(which(is.na(col_rcc_from_uncoded)))
 } else {
   comp_val_result[i] <- NA
   comp_na_result[i] <- NA
 }
}
table(comp_val_result, useNA = "always")
table(comp_na_result, useNA = "always")

head(colnames(rcc)[which(!comp_val_result)], 10)
head(colnames(rcc)[which(!comp_na_result)], 10)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
