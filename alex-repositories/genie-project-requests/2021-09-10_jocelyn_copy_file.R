# Description: Copy sponsored project data file to delivery folder on Synapse.
# Author: Haley Hunter-Zinck
# Date: September 10, 2021
# Request:
#   Source: slack message from Jocelyn
#   Date received: 2021-09-10
#   Message: HI Haley and Xindi.  The USCF BRCA data are in and located here:  https://www.synapse.org/#!Synapse:syn26141809
#     Can you move them to here:  https://www.synapse.org/#!Synapse:syn26023438
#     Or better yet, copy them also to here:  https://www.synapse.org/#!Synapse:syn26023438
#     Thanks so much!

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(synapser)
synLogin()

# synapse
synid_file_data <- "syn26141809"
synid_folder_new <- "syn26023438"

# functions ----------------------------

save_to_synapse <- function(file_name, parent_id, prov_name = "", prov_desc = "", prov_used = "", prov_exec = "") {
  file <- File(path = file_name, parentId = parent_id)
  act <- Activity(name = prov_name,
                  description = prov_desc,
                  used = prov_used,
                  executed = prov_exec)
  file <- synStore(file, activity = act)
  
  return(T)
}


# main ----------------------------

entity <- synGet(synid_file_data)
save_to_synapse(file_name = entity$path, 
                parent_id = synid_folder_new, 
                prov_name = "copy file", 
                prov_desc = "copy file to Synapse directory for delivery", 
                prov_used = synid_file_data, 
                prov_exec = "https://github.com/hhunterzinck/genie_requests/2021-09-10_jocelyn_copy_file.R")


# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
