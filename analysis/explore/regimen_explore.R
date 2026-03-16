# Description: A sandbox for looking at and playing with the datasets.
#   This script will never be a part of the final analysis workflow.
# Author: Alex Paynter

library(cli) # prevents an error on version reliance.
library(readr)
library(vctrs)
library(rlang)
library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(janitor)
library(glue)
library(genieBPC)
library(ggplot2)
library(sunburstR)
library(lobstr) # just for lobstr::tree - more readable playing

purrr::walk(.x = here("R", dir(here("R"))), .f = source)

data_list <- readr::read_rds(here("data-raw", "data_list.rds"))
