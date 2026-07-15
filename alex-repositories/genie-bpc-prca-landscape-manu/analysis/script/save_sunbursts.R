library(sunburstR)
library(here)
library(readr)
library(dplyr)
library(stringr)
library(genieBPC)
library(magrittr)
library(glue)
library(cli)

purrr::walk(.x = here("R", dir(here("R"))),
            .f = source)

data_list <- readRDS(
  file = here("data-raw", "genieBPC-style", "data_list.rds")
)

# As usual this function is a hive of bugs:
# create_analytic_cohort(data_synapse = data_list$Prostate_v1.2)
dl_renamed <- rename_dl_for_sunburst(data_list)
data_list$Prostate_v1.2 <- data_list$Prostate_v1.2[c("pt_char",
                                                     "ca_dx_index",
                                                     "ca_drugs",
                                                     "cpt")]

dft_trt_hist_full <- drug_regimen_sunburst(
  data_synapse = data_list$Prostate_v1.2,
  data_cohort = dl_renamed,
  max_n_regimens = 3)
dft_trt_hist_full <- dft_trt_hist_full$treatment_history

js_sun_full <- plot_regimen_sunburst(
  dft_trt_hist_full, 
  seed = 98,
)
saveRDS(file = here("data", "sunburst_plots", "full.rds"),
        object = js_sun_full)






# Need to pull some data to filter this second one:
read_wrap <- function(p) {
  read_csv(file = here("data-raw", p), show_col_types = F)
}

dft_ca_ind <- read_wrap("cancer_level_dataset_index.csv")
dft_med_onc <- read_wrap("med_onc_note_level_dataset.csv")

crpc_times <- make_cast_status_block(
  read_wrap("med_onc_note_level_dataset.csv"),
  read_wrap("cancer_level_dataset_index.csv"),
  remove_hspc_after_crpc = T) 
crpc_times %<>%
  filter(md_cast_status_f %in% "Castrate-Resistant") %>% 
  select(record_id, tt_y = dx_block_start) %>%
  mutate(tt_d = tt_y*365.25)


dl_crpc <- filter_dl_by_pt(
  data_list, 
  prog_timing = crpc_times
)


# Repeat the sunburst code:
dft_trt_hist_crpc <- drug_regimen_sunburst(
  data_synapse = data_list$Prostate_v1.2,
  data_cohort = dl_crpc,
  max_n_regimens = 3)
dft_trt_hist_crpc <- dft_trt_hist_crpc$treatment_history

js_sun_crpc <- plot_regimen_sunburst(
  dft_trt_hist_crpc, 
  seed = 029348,
)

saveRDS(file = here("data", "sunburst_plots", "crpc.rds"),
        object = js_sun_crpc)








dft_drug <- readr::read_csv(here("data", "drug.csv"))

# Using my drugs dataset first just to get a rough idea on the right answers:
# dft_abi_enza_doce <- 


dft_abi_enza_doce <- get_limited_drug_data(
  dft_drug, 
  drug_str = c(
    "Abiraterone Acetate", 
    "Docetaxel",
    "Enzalutamide"
  )
)

dft_abi_enza_doce_trt_hist <- make_sunburst_input(
  dat = dft_abi_enza_doce,
  var = "regimen_drugs",
  order_var = "regimen_number",
  max_depth = NULL
)

js_sun_aed <- plot_regimen_sunburst(
  dft_abi_enza_doce_trt_hist, 
  pal = (viridisLite::plasma(
    n = sum(choose(3, 1:3)),
    begin = 0.1, end = 0.7,
  ))
)
saveRDS(file = here("data", "sunburst_plots", "abi_enza_doce.rds"),
        object = js_sun_aed)


# Same thing but remove the duplicates, e.g. if a participant
#   has Enza followed by Enza - who cares?
dft_abi_enza_doce_trt_hist_no_dupes <- make_sunburst_input(
  dat = dft_abi_enza_doce,
  var = "regimen_drugs",
  order_var = "regimen_number",
  max_depth = NULL,
  remove_dupes = T
)

js_sun_aed_no_dupes <- plot_regimen_sunburst(
  dft_abi_enza_doce_trt_hist_no_dupes, 
  seed = 94,
  pal = (viridisLite::magma(
    n = sum(choose(3, 1:3)),
    begin = 0.1, end = 0.7,
  ))
)
saveRDS(
  file = here("data", "sunburst_plots", "abi_enza_doce_no_dupes.rds"),
  object = js_sun_aed_no_dupes
)





# Simple = Monotherapies only, no duplicates.
dft_abi_enza_doce_simple <- dft_abi_enza_doce %>%
  filter(regimen_drugs %in% c("Abiraterone Acetate", "Docetaxel", "Enzalutamide")) %>%
  make_sunburst_input(
    dat = .,
    var = "regimen_drugs",
    order_var = "regimen_number",
    max_depth = NULL,
    remove_dupes = T
  )

js_sun_aed_simple <- plot_regimen_sunburst(
  dft_abi_enza_doce_simple,
  seed = 1,
  pal = c('#3B0F70', '#B63679', '#F76F5C')[c(3,1,2)]
#   pal = c('#D76D5C', '#36136D', '#9D3976')
)

js_sun_aed_simple
saveRDS(
  file = here("data", "sunburst_plots", "abi_enza_doce_simple.rds"),
  object = js_sun_aed_simple
)








# Update From May 8:  Create a plot similar to abi/enza/doce except with 
#   Radium 223 and docetaxel.

dft_rad_doce <- get_limited_drug_data(
  dft_drug, 
  drug_str = c(
    "Docetaxel",
    "Radium RA 223 Dichloride"
  )
)

dft_rad_doce_hist_no_dupes <- make_sunburst_input(
  dat = dft_rad_doce,
  var = "regimen_drugs",
  order_var = "regimen_number",
  max_depth = NULL,
  remove_dupes = T
)

js_sun_rad_doce_no_dupes <- plot_regimen_sunburst(
  dft_rad_doce_hist_no_dupes,
  seed = 94,
  pal = (viridisLite::mako(
    n = sum(choose(2, 1:2)),
    begin = 0.1, end = 0.7,
  ))
)

saveRDS(
  file = here("data", "sunburst_plots", "radium_doce_no_dupes.rds"),
  object = js_sun_rad_doce_no_dupes
)







