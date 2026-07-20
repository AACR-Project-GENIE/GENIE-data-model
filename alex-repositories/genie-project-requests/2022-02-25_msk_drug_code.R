# Description: check upload for problematic drug record.  
# Author: Haley Hunter-Zinck
# Date: 2022-02-25

library(synapser)
synLogin()

# synapse
synid_file_dd <- "syn25468849"
synid_file_msk_panc <- "syn25541828"

real_code <- "49135"

data <- read.csv(synGet(synid_file_msk_panc)$path)
dd <- read.csv(synGet(synid_file_dd)$path, check.names = F)

entered_code <- unlist(data %>%
  filter(record_id == "GENIE-MSK-P-0032671" & redcap_repeat_instrument == 'ca_directed_drugs' & redcap_repeat_instance == 2) %>%
  select(drugs_drug_4))
  
choices <- unlist(dd %>%
  filter(`Variable / Field Name` == "drugs_drug_4") %>%
  select(`Choices, Calculations, OR Slider Labels`))

print(glue("Found entered code ({entered_code}) in choices: {grepl(pattern = entered_code, x = choices)}"))
print(glue("Found verified code ({real_code}) in choices: {grepl(pattern = real_code, x = choices)}"))
