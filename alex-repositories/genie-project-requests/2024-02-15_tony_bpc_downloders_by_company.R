library(dplyr)
library(glue)
library(stringr)
library(synapser)
synLogin()

user_info_table <- read.csv("~/Downloads/BPC_cBioPortal_access.csv")
downloader_table <- read.csv("~/Downloads/GEN-1162 user list request.tsv",sep = "\t")

# NSCLC
nsclc_downloader_info <- downloader_table %>% 
  filter(COHORT=="NSCLC") %>%
  select(USERNAMES, EMAILS)

synapse_username <- unlist(strsplit(nsclc_downloader_info$USERNAMES,","))
# nsclc_email_list <- unlist(strsplit(nsclc_downloader_info$EMAILS,","))

nsclc_downloader_table <- data.frame(synapse_username)
nsclc_downloader_table$cohort <- "NSCLC"

nsclc_final <- merge(nsclc_downloader_table, user_info_table, by = "synapse_username", all.x = TRUE)
write.csv(nsclc_final,file='GEN-1162_NSCLC_user_list.csv',row.names = FALSE,na = "")


# CRC
crc_downloader_info <- downloader_table %>% 
  filter(COHORT=="CRC") %>%
  select(USERNAMES, EMAILS)

synapse_username <- unlist(strsplit(crc_downloader_info$USERNAMES,","))

crc_downloader_table <- data.frame(synapse_username)
crc_downloader_table$cohort <- "CRC"

crc_final <- merge(crc_downloader_table, user_info_table, by = "synapse_username", all.x = TRUE)
write.csv(crc_final,file='GEN-1162_CRC_user_list.csv',row.names = FALSE,na = "")


# BrCa
brca_downloader_info <- downloader_table %>% 
  filter(COHORT=="BrCa") %>%
  select(USERNAMES, EMAILS)

synapse_username <- unlist(strsplit(brca_downloader_info$USERNAMES,","))

brca_downloader_table <- data.frame(synapse_username)
brca_downloader_table$cohort <- "BrCa"

brca_final <- merge(brca_downloader_table, user_info_table, by = "synapse_username", all.x = TRUE)
write.csv(brca_final,file='GEN-1162_BrCa_user_list.csv',row.names = FALSE,na = "")

# Prostate
prostate_downloader_info <- downloader_table %>% 
  filter(COHORT=="Prostate") %>%
  select(USERNAMES, EMAILS)

synapse_username <- unlist(strsplit(prostate_downloader_info$USERNAMES,","))

prostate_downloader_table <- data.frame(synapse_username)
prostate_downloader_table$cohort <- "Prostate"

prostate_final <- merge(prostate_downloader_table, user_info_table, by = "synapse_username", all.x = TRUE)
write.csv(prostate_final,file='GEN-1162_Prostate_user_list.csv',row.names = FALSE,na = "")

# PANC
panc_downloader_info <- downloader_table %>% 
  filter(COHORT=="PANC") %>%
  select(USERNAMES, EMAILS)

synapse_username <- unlist(strsplit(panc_downloader_info$USERNAMES,","))

panc_downloader_table <- data.frame(synapse_username)
panc_downloader_table$cohort <- "PANC"

panc_final <- merge(panc_downloader_table, user_info_table, by = "synapse_username", all.x = TRUE)
write.csv(panc_final,file='GEN-1162_PANC_user_list.csv',row.names = FALSE,na = "")

# BLADDER
bladder_downloader_info <- downloader_table %>% 
  filter(COHORT=="BLADDER") %>%
  select(USERNAMES, EMAILS)

synapse_username <- unlist(strsplit(bladder_downloader_info$USERNAMES,","))

bladder_downloader_table <- data.frame(synapse_username)
bladder_downloader_table$cohort <- "BLADDER"

bladder_final <- merge(bladder_downloader_table, user_info_table, by = "synapse_username", all.x = TRUE)
write.csv(bladder_final,file='GEN-1162_BLADDER_user_list.csv',row.names = FALSE,na = "")
