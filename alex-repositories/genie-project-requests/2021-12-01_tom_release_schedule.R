# Description: Automate release schedule dates for main GENIE by calculating
#   the #nd ***day of each month between a specified date range.  
# Author: Haley Hunter-Zinck
# Date: 2021-12-01

# pre-setup  ---------------------------

library(optparse)

waitifnot <- function(cond, msg) {
  if (!cond) {
    
    for (str in msg) {
      message(str)
    }
    message("Press control-C to exit and try again.")
    
    while(T) {}
  }
}

# user input ----------------------------

option_list <- list( 
  make_option(c("-s", "--date_start"), type = "character",
              help="start date in YYYY-MM-DD format"),
  make_option(c("-e", "--date_end"), type = "character",
              help="end date in YYYY-MM-DD format"),
  make_option(c("-n", "--number_occurrence"), type = "integer",
              help="Number of the occurrence of a day of week in a month"),
  make_option(c("-d", "--day_of_week"), type = "character",
              help="Day of the week, either full day or 3-letter abbreviation")
)
opt <- parse_args(OptionParser(option_list=option_list))
waitifnot(!is.null(opt$date_start) && !is.null(opt$date_end) && !is.null(opt$number_occurrence) && !is.null(opt$day_of_week),
          msg = "Rscript 2021-12-01_tom_release_schedule.R -h")

date_start <- opt$date_start
date_end <- opt$date_end
n_occ <- opt$number_occurrence
dow <- opt$day_of_week

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(lubridate)

# functions ----------------------------

#' Get the first date of a month corresponding to the date.
#' 
#' @param @date Date as a string in YYYY-MM-DD format
#' @return First day of the month
floor_date <- function(date) {
  return(ymd(glue("{year(date)}-{month(date)}-01")))
}

#' Get the last date of a month corresponding to the date.
#' 
#' @param @date Date as a string in YYYY-MM-DD format
#' @return Last day of the month
ceiling_date <- function(date) {
  
  y_date <- year(date)
  m_date <- month(date)
  d_date <- day(date)
  next_first <- ""
  
  if (m_date == 12) {
    next_first <- ymd(glue("{y_date + 1}-01-01"))
  } else {
    next_first <- ymd(glue("{y_date}-{m_date + 1}-01"))
  }
  
  return(next_first - 1)
}

capitalize <- function(str) {
  first <- substr(str, 1, 1)
  rest <- substr(str, 2, nchar(str))
  return(glue("{toupper(first)}{tolower(rest)}"))
}

format_dow <- function(dow) {
  return(substr(capitalize(dow), 1, 3))
}

get_occ_dow <- function(date_start, date_end, n_occ, dow) {
  
  # floor dates to month
  floor_start <- floor_date(date_start)
  ceiling_end <- ceiling_date(date_end)
  
  # get dow of all dates
  seq_date <- floor_start + 1:as.integer(ceiling_end - floor_start)
  obj <- data.frame(date = seq_date, 
                    wd = wday(seq_date, label = T, abbr = T) ,
                    month = month(seq_date))
  
  ordering <- obj %>% 
    filter(wd == format_dow(dow)) %>%
    group_by(month) %>%
    group_rows()
  idx <- unlist(lapply(ordering, function(x) {return(x[n_occ])}))
  
  
  targets <- (obj %>% 
    filter(wd == format_dow(dow)) %>%
    select(date))[["date"]][idx]
  
  return(targets[targets >= date_start & targets <= date_end])
}

# main ----------------------------

targets <- get_occ_dow(date_start = date_start, 
            date_end = date_end, 
            n_occ = n_occ, 
            dow = dow)

# close out ----------------------------

str_targets <- paste0("\n", paste0(targets, collapse = "\n"))
print(glue("{dow} number {n_occ} in month(s) between {date_start} and {date_end}: {str_targets}"))

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
