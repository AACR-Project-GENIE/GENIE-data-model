# Description: check new case selection files.
# Author: Haley Hunter-Zinck
# Date: 2021-11-10
# Request: modify case selection with two fixes
#   1. randomize sdv and irr so that QA managers can go down the list in order and periodically to these checks
#   2. number of sdv cases is 20% of production total on top of pressure cases

library(dplyr)
library(glue)

#data <- read.csv("nsclc_dfci_phase2_case_selection.csv", na.strings = "")
#data <- read.csv("nsclc_msk_phase2_case_selection.csv", na.strings = "")
#data <- read.csv("nsclc_uhn_phase2_case_selection.csv", na.strings = "")
data <- read.csv("nsclc_vicc_phase2_case_selection.csv", na.strings = "")

head(data)
n_prod <- data %>% filter(category == "production") %>% count()

n_irr <- data %>% filter(irr == "irr") %>% count()
n_sdv <- data %>% filter(sdv == "sdv") %>% count()
n_pressure <- data %>% filter(pressure == "pressure") %>% count()
n_pressure_sdv <- data %>% filter(pressure == "pressure" & sdv == "sdv") %>% count()
n_sdv_irr <- data %>% filter(sdv == "sdv" & irr == "irr") %>% count()

print(glue("production = {n_prod}"))
print(glue("pressure = {n_pressure}"))
print(glue("pressure & sdv = {n_pressure_sdv}"))
print(glue("sdv (excluding pressure) = {n_sdv - n_pressure} ({round((n_sdv - n_pressure) / n_prod * 100, 2)}%)"))
print(glue("irr = {n_irr} ({round(n_irr / n_prod * 100, 2)}%)"))
print(glue("sdv & irr: {n_sdv_irr}"))
