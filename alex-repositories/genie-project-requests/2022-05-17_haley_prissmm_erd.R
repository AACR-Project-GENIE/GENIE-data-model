# Description: Create a local postgres database based on a REDCap data dictionary.
# Author: Haley Hunter-Zinck
# Date: 2022-05-17

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)

# synapse
synid_file_dd <- "syn26344720"
verbose <- T

# sql
username = "postgres"
# note: password cached in ~/.pgpass
database <- "bpc"
schema <- "prissmm_3_7_5"
cmd_psql_statement <- glue("psql -U {username} -d {database} -c")
cmd_psql_file <- glue("psql -U {username} -d {database} -f")

# parameters
map_primary_repeat <- c("cohort" = "VARCHAR", 
                 "record_id" = "VARCHAR", 
                 "redcap_repeat_instance" = "INTEGER")
map_primary_nonrepeat <- c("cohort" = "VARCHAR", 
                        "record_id" = "VARCHAR")
nonrepeat_instruments <- c("curation_initiation_eligibility", "patient_characteristics", "curation_completion", "quality_assurance")

# functions ----------------------------

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
#' @param comment.char character designating comment lines to ignore
#' @return data frame
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
                   header = header, comment.char = comment.char, colClasses = colClasses)
  return(data)
}

create_schema <- function(schema, drop_if_exists = F) {
  
  cmd_sql_prefix <- ""
  if (drop_if_exists) {
    cmd_sql_prefix <- glue("DROP SCHEMA IF EXISTS \"{schema}\" CASCADE;")
  }
  cmd_sql_body <- glue("CREATE SCHEMA IF NOT EXISTS \"{schema}\";")
  cmd_sql_suffix <- ""
  
  cmd_sql <- glue("{cmd_sql_prefix}\n{cmd_sql_body}\n{cmd_sql_suffix}")
  cmd_psql <- glue("{cmd_psql_statement} '{cmd_sql}'")
  system(cmd_psql)
  
  return(T)
}

create_table <- function(var_names, var_types, table_name, schema = "public", 
                                   drop_if_exists = F) {
  
  cmd_sql_prefix_a <- glue("SET search_path to \"{schema}\";")
  cmd_sql_prefix_b <- ""
  if(drop_if_exists) {
    cmd_sql_prefix_b <- glue("DROP TABLE IF EXISTS {table_name};")
  }
  cmd_sql_prefix_c <- glue("CREATE TABLE IF NOT EXISTS {table_name} (")
  cmd_sql_prefix <- glue("{cmd_sql_prefix_a} \n{cmd_sql_prefix_b}\n{cmd_sql_prefix_c}")
  
  cmd_sql_body <- ""
  for (i in seq_len(length(var_names))) {
    
    type = var_types[i]
    
    if (i == 1) {
      cmd_sql_column <- glue("\n{var_names[i]} {var_types[i]}")
    } else {
      cmd_sql_column <- glue(",\n{var_names[i]} {var_types[i]}")
    }
    cmd_sql_body <- glue("{cmd_sql_body} {cmd_sql_column}")
  }
  cmd_sql_suffix <- ")"
  
  cmd_sql <- glue("{cmd_sql_prefix}\n{cmd_sql_body}\n{cmd_sql_suffix}")
  cmd_psql <- glue("{cmd_psql_statement} '{cmd_sql}'")
  system(cmd_psql)
  
  return(table_name)
}

get_sql_var_types <- function(val_types) {
  var_types_sql <- rep("VARCHAR", length(val_types))
  var_types_sql[which(val_types == "integer")] <- "INTEGER"
  var_types_sql[which(val_types == "number")] <- "NUMERIC"
  var_types_sql[which(val_types == "datetime_seconds_mdy")] <- "TIMESTAMP"
  var_types_sql[which(val_types == "date_mdy")] <- "DATE"
  return(var_types_sql)
}

# synapse login --------------------

status <- synLogin()

# read ----------------------------

df_dd <- get_synapse_entity_data_in_csv(synid_file_dd)

# main ----------------------------

instruments <- as.character(unlist(df_dd %>% 
  select(`Form Name`) %>%
  distinct()))

# create schena
status <- create_schema(schema = schema, drop_if_exists = T)

for (instrument in instruments) {
  
  var_names <- as.character(unlist(df_dd %>% 
                                     filter(`Form Name` == instrument) %>%
                                     select(`Variable / Field Name`)))
  var_types_val <- as.character(unlist(df_dd %>% 
                         filter(`Form Name` == instrument) %>%
                         select(`Text Validation Type OR Show Slider Number`)))
  var_types_sql <- get_sql_var_types(val_types = var_types_val)
  
  # add primary keys, if not present
  map_primary <- map_primary_repeat
  if (is.element(instrument, nonrepeat_instruments)) {
    map_primary <- map_primary_nonrepeat
  }
  idx_rm <- which(is.element(var_names, names(map_primary)))
  if (length(idx_rm)) {
    var_names <- var_names[-idx_rm]
    var_types_sql <- var_types_sql[-idx_rm]
  }
  var_names <- append(names(map_primary), var_names)
  var_types_sql <- append(as.character(map_primary), var_types_sql)
  
  table_name <- create_table(var_names = var_names, 
                            var_types = var_types_sql, 
                            table_name = instrument, 
                            schema = schema, 
                            drop_if_exists = T)
}

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
