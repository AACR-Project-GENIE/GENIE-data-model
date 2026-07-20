library(dplyr)
library(glue)
library(synapser)
synLogin()

cohort <- 'NSCLC'
dd_synid <- 'syn26469281'
data_dictionary <- read.csv(synGet(dd_synid)$path)[,c(1,2,5,4)]
colnames(data_dictionary) <- c('variable','instrument','label','type')

site <- 'VICC'
report_synid <- 'syn26529604'
warning_report <- read.csv(synGet(report_synid)$path)

result <- warning_report %>% 
  filter(check_no == 9) %>%
  select(-c(1,2,3,6,7,8,10)) %>%
  rename(variable=column_name) %>%
  left_join(data_dictionary, by='variable') %>%
  select(c(3,7,8))

file_name <-glue('{tolower(cohort)}_{tolower(site)}_empty_columns_list.csv')
ent_name <- glue('{tolower(site)}_empty_columns_list.csv')
write.csv(result, file=file_name)

#upload to Synapse
ent <- File(path=file_name, name=ent_name, parent='syn51730508')
synStore(ent, used=c(report_synid,dd_synid))

