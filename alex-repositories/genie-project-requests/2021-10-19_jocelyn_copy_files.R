# Description: Move data on Synapse for BrCa sponsored project
# Author: Haley Hunter-Zinck
# Date: 2021-10-19
# Request: 
#     Source: Freedcamp task from Jocelyn: https://freedcamp.com/view/3082480/tasks/42846604
#     Date requested: 2021-10-19
#     Msg: move Duke data

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(synapser)
synLogin()

# synapse
synid_file_duke_data <- "syn26341643"
synid_file_ucsf_data <- "syn26376139"
synid_folder_dest <- "syn26023438"

# provenance
prov_name <- "copy file"
prov_desc <- "copy file to Synapse directory for delivery"
prov_exec <- "https://github.com/hhunterzinck/genie_requests/blob/main/2021-10-19_jocelyn_copy_files.R"

# functions ----------------------------

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

# main ----------------------------

synid_folder_children <- get_synapse_folder_children(synapse_id = synid_folder_dest, 
                                                                 include_types=list("file"))

# duke
ent_duke <- synGet(synid_file_duke_data)
save_to_synapse(path = ent_duke$path, 
                parent_id = synid_folder_dest,
                file_name = grep(pattern = "DUKE", x = names(synid_folder_children), value = T),
                prov_name = prov_name, 
                prov_desc = prov_desc, 
                prov_used = synid_file_duke_data, 
                prov_exec = prov_exec)

# ucsf
ent_ucsf <- synGet(synid_file_ucsf_data)
save_to_synapse(path = ent_ucsf$path, 
                parent_id = synid_folder_dest,
                file_name = grep(pattern = "UCSF", x = names(synid_folder_children), value = T),
                prov_name = prov_name, 
                prov_desc = prov_desc, 
                prov_used = synid_file_ucsf_data, 
                prov_exec = prov_exec)
  
# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
