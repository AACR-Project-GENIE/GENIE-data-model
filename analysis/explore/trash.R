
library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_all <- readr::read_rds(
  here('data', 'all_dat.rds')
)

test <- dft_all %>%
  filter(dat_name %in% "ca_ind") %>%
  filter(str_detect(cohort, "CRC")) %>%
  pull(dat) %>%
  `[[`(.,1) 

discrepant_pt_list <- c(
  'GENIE-DFCI-000615',
  'GENIE-DFCI-001298',
  'GENIE-DFCI-004881',
  'GENIE-DFCI-007863',
  'GENIE-DFCI-008007',
  'GENIE-DFCI-008780',
  'GENIE-DFCI-008987',
  'GENIE-DFCI-009011',
  'GENIE-DFCI-009998',
  'GENIE-DFCI-010084',
  'GENIE-DFCI-010828',
  'GENIE-DFCI-011273',
  'GENIE-DFCI-011501',
  'GENIE-DFCI-011505',
  'GENIE-DFCI-012711',
  'GENIE-DFCI-037726',
  'GENIE-DFCI-051363',
  'GENIE-DFCI-077129',
  'GENIE-DFCI-089493',
  'GENIE-DFCI-092198',
  'GENIE-MSK-P-0005644',
  'GENIE-MSK-P-0005942',
  'GENIE-MSK-P-0006028',
  'GENIE-MSK-P-0006198',
  'GENIE-MSK-P-0006640',
  'GENIE-MSK-P-0007007',
  'GENIE-MSK-P-0007345',
  'GENIE-MSK-P-0009102',
  'GENIE-MSK-P-0010126',
  'GENIE-MSK-P-0010172',
  'GENIE-MSK-P-0010337',
  'GENIE-MSK-P-0012562',
  'GENIE-MSK-P-0016746',
  'GENIE-MSK-P-0016781',
  'GENIE-MSK-P-0017938',
  'GENIE-MSK-P-0019128',
  'GENIE-MSK-P-0019344',
  'GENIE-MSK-P-0021628',
  'GENIE-MSK-P-0022129',
  'GENIE-MSK-P-0022527',
  'GENIE-MSK-P-0024401',
  'GENIE-MSK-P-0024900',
  'GENIE-VICC-644093'
)

discrepant_pt_list <- c(
  "GENIE-DFCI-004881",
  "GENIE-DFCI-008780",
  "GENIE-MSK-P-0010581",
  "GENIE-DFCI-001303",
  "GENIE-DFCI-111505"
)


ggplot(
  (test %>% mutate(disc = record_id %in% discrepant_pt_list)),
  aes(x = age_dx, y = 1, color = disc)
) + 
  geom_jitter(height = 1, width = 0) + 
  theme_bw()

test %>%
  filter(record_id %in% discrepant_pt_list) 
