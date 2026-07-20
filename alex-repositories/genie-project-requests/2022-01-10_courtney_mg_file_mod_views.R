# Description: Create file views of main GENIE input folders for each site and 
#     paste a relevant summarizing query in each input folder's wiki.
# Author: Haley Hunter-Zinck
# Date: 2022-01-07
# Query: SELECT date(FROM_UNIXTIME(modifiedOn/1000)) AS modifiedDate, modifiedBy, COUNT(*) AS nFileModified FROM syn26709679 GROUP BY modifiedDate, modifiedBy ORDER BY modifiedDate DESC, modifiedBy ASC

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
  make_option(c("-f", "--synid_folder_input"), type = "character",
              help="Synapse ID of input file"),
  make_option(c("-p", "--synid_project_view"), type = "character",
              help="Synapse ID of output folder"),
  make_option(c("-c", "--center"), type = "character",
              help="Center abbreviation"),
  make_option(c("-v", "--verbose"), action="store_true", default = FALSE, 
              help="Output script messages to the user.")
)
opt <- parse_args(OptionParser(option_list=option_list))
waitifnot(!is.null(opt$synid_folder_input) && !is.null(opt$synid_project_view) && !is.null(opt$center),
          msg = "Rscript 2022-01-07_courtney_mg_file_mod_views.R -h")

synid_folder_input <- opt$synid_folder_input
synid_project_view <- opt$synid_project_view
center <- opt$center
verbose <- opt$verbose

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

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
#' @return data frame
get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA,
                                           sep = ",", 
                                           na.strings = c("NA"), 
                                           header = T,
                                           check_names = F) {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = check_names,
                   header = header)
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

#' Create a Synapse file view in a given project with a given scope for the
#' user specified entity types.
#' 
#' @param name the name of the Entity View Table object
#' @param synid_folder_scope Synapse ID of folder the file view should cover
#' @param synid_project_parent Synapse ID of project in which the file view 
#' should be stored
#' @param include_entity_type a vector of entity types to include in the view
create_synapse_file_view <- function(name, 
                                     synid_folder_scope, 
                                     synid_project_parent, 
                                     include_entity_types = c(EntityViewType$FILE,
                                                              EntityViewType$FOLDER),
                                     add_annotation_columns = F) {
  entity_view <- EntityViewSchema(name = name, 
                                  parent = synid_project_parent, 
                                  scopes = synid_folder_scope,
                                  includeEntityTypes = include_entity_types,
                                  addAnnotationColumns = add_annotation_columns)
  synid_view <- synStore(entity_view)
  return(synid_view$properties$id)
}

#' Replace content in a Synapse folder wiki.  
#' 
#' @param synid_folder Synapse ID of folder or project
#' @param title Title of the Wiki Page
#' @param content Content of the Wiki page to place in markdown format.
#' @return Synapse ID of folder or project where wiki is loaded
create_synapse_wiki <- function(synid_folder, content) {
  entity_wiki <- Wiki(owner = synid_folder, markdown = content)
  wiki <- synStore(entity_wiki)
  return(wiki$ownerId)
}

# main ----------------------------

# create file view
synid_table_view <- create_synapse_file_view(name = glue("{tolower(center)}-input-files"), 
                         synid_folder_scope = synid_folder_input, 
                         synid_project_parent = synid_project_view, 
                         include_entity_types = c(EntityViewType$FILE))

wiki_desc <- "## Upload Summary\nThe following table shows the number of uploaded or modified files by Synapse user and date with most recent dates first.  ***If new file counts do not immediately appear, try refreshing the page.***"
wiki_content <- glue("[wiki_desc]\n\n${synapsetable?query=SELECT date%28FROM%5FUNIXTIME%28modifiedOn/1000%29%29 AS modifiedDate%2C modifiedBy%2C COUNT%28%2A%29 AS nFileModified FROM [synid_table_view] GROUP BY modifiedDate%2C modifiedBy ORDER BY modifiedDate DESC%2C modifiedBy ASC&showquery=false}", 
                     .open = "[", .close = "]")
synid_dest <- create_synapse_wiki(synid_folder = synid_folder_input, content = wiki_content) 

# close out ----------------------------

if (verbose) {
  print(glue("Wiki stored at {synid_dest}."))
  toc = as.double(Sys.time())
  print(glue("Runtime: {round(toc - tic)} s"))
}


