library(dplyr)
library(glue)
library(stringr)
library(synapser)
synLogin()

cohort <- 'NSCLC'

dd_synid <- 'syn26469281'
data_dictionary <- read.csv(synGet(dd_synid)$path)[,c(1,2,5,4)]
colnames(data_dictionary) <- c('variable','instrument','label','type')

report_synid <- 'syn26533170' 
warning_report <- read.csv(synGet(report_synid)$path)

selected_site <- 'MSK'

result <- warning_report %>% 
  filter(check_no == 35, site == selected_site, !str_detect(synapse_name, "^ARCHIVE")) %>%
  select(-c(1,2,3,6,7,8,10,11,12,13)) %>%
  rename(variable=column_name) %>%
  left_join(data_dictionary, by='variable')

file_name <-glue('{tolower(cohort)}_{tolower(selected_site)}_empty_columns_list_check35only.csv')
ent_name <- glue('{tolower(selected_site)}_empty_columns_list_check35only.csv')
write.csv(result, file=file_name)

# upload to Synapse with provanance setup
ent <- File(path=file_name, name=ent_name, parent='syn51730508')
synStore(ent, used=c(report_synid,dd_synid))
