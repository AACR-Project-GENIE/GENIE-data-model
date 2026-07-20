

library(readxl)
library(pdftools)
library(pdftables)
library(dplyr)
library(magrittr)
library(purrr)
library(stringr)

path_guide <- here('data-raw', 'manual', 'guideline')

ex_path <- here(path_guide, 'pancreatic_2.2023_active_061923.pdf')

vec_pdf <- pdf_text(
  ex_path
)

extract_pdf_strings <- function(
    v_pdf, 
    str_to_match, 
    return_strings = F
) {
  res <- map(
    .x = v_pdf,
    .f = \(x) {
      str_extract_all(x, pattern = str_to_match)
    }
  )
  
  res <- unlist(res)
  
  return(res)
  
}

extract_pdf_strings(vec_pdf, str_to_match = "[cC]ape[a-zA-Z]+")

dft_nccn <- tibble(
  file = dir_ls(path_guide)
)

dft_nccn %<>%
  mutate(
    important_part = str_extract(file, pattern = "_[123].[0-9]{4}.*[0-9]{6}"),
    important_part = str_sub(important_part, start = 4),
    active_year = str_sub(important_part, end = 4),
    date = str_sub(important_part, start = 13, end = 18)
  ) %>%
  select(date, file, active_year) %>%
  mutate(date = lubridate::mdy(date)) %>%
  arrange(date)

dft_nccn %<>%
  mutate(
    pdf_vec = purrr::map(
      .x = file,
      .f = pdf_text
    )
  )

# mmmk, some of the early ones didn't work , who cares.

dft_nccn %<>%
  mutate(
    broad_string_matches = purrr::map(
      .x = pdf_vec,
      .f = \(z) {
        extract_pdf_strings(z, str_to_match = "[Cc]ape[a-zA-Z]+")
      }
    )
  )

# Unique strings discovered:
unique(Reduce(c, dft_nccn$broad_string_matches))

# The only one I dont want is "Capello" (an author)

dft_nccn %<>%
  mutate(
    string_matches = purrr::map(
      .x = broad_string_matches,
      .f = \(z) z[!(z %in% "Capello")]
    ),
    n_text_matches = purrr::map_dbl(
      .x = string_matches,
      .f = length
    )
  )

dft_nccn_sum <- select(
  dft_nccn,
  date, file, active_year, string_matches, n_text_matches
)

readr::write_rds(
  dft_nccn_sum,
  here('data', 'nccn_panc_capecitabine_sum.rds')
)

ggplot(
  dft_nccn_sum,
  aes(x = date, y = n_text_matches)
) + 
  geom_point() + 
  labs(
    x = "Guideline date (file name)",
    y = "Number of text matches",
    title = "String search of PDF files",
    subtitle = "Strings matching 'Cape*', except 'Capello'"
  ) + 
  theme_bw() + 
  theme(
    plot.title.position = "plot"
  )
        
        
vec_dg %>% str # nice, each entry of the vector is a page.

vec_dg_vars <- str_extract_all(
  string = vec_dg,
  pattern = "\\n[:space:]*\\[.*\\]"
) 

# Little cleanup on these first:
vec_dg_vars %<>%
  map(.x = ., .f = \(a) str_replace_all(string = a, pattern = "\\n", replacement = "")) %>%
  # break up comma separated variables:
  map(
    .x = ., 
    .f = \(a) {
      unlist(str_split(a, pattern = ","))
    }
  ) %>%
  map(.x = ., str_trim)
# Still need to do the more complex "range" variables.


dat_dg_vars <- purrr::map2(
  .x = vec_dg_vars,
  .y = seq_along(vec_dg_vars),
  .f = \(x,y) tibble(page = y, var = x)
) %>%
  bind_rows(.)