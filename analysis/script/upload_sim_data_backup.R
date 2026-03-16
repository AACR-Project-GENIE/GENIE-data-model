upload_synid <- "syn52319716" 

library(purrr)
library(here)
library(fs)
library(synapser)

purrr::walk(.x = fs::dir_ls('R'), .f = source)

synLogin()

cli::cli_abort("Does not work, need to troubleshoot.  Manually upload for now.")
# synapser::Folder(here("sim", "run_methods"),
#                  parent = upload_synid) %>%
#   synStore()
