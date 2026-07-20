library(fs); library(purrr); library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

library(pdftools)
library(pdftables)
library(stringi)

path_guide <- here('data-raw', 'manual', 'nccn_guideline', 'rectal')

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

# Year cutoff when these failed seems roughly the same as panc
dft_nccn %<>%
  filter(
    date > ymd("2001-01-01")
  )


dft_nccn %<>%
  mutate(
    pdf_vec = purrr::map(
      .x = file,
      .f = pdf_text
    )
  )

# illustrative example of the regex we're using:
# stri_count_regex(c("a bcde.*678", "ab", "a.  \n b \n"), pattern = "[a-zA-Z]")

dft_nccn %<>%
  mutate(
    alpha_char_count = purrr::map_dbl(
      .x = pdf_vec,
      .f = \(z) {
        sum(stri_count_regex(
          z, "[a-zA-Z]"
        ))
      }
    ),
    word_count = purrr::map_dbl(
      .x = pdf_vec,
      .f = \(z) {
        sum(stri_count_regex(
          z, "[a-zA-Z[:punct:]]+"
        ))
      }
    )
  )

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
    ),
    n_matches_capeox = purrr::map_dbl(
      .x = string_matches,
      .f = \(z) length(z[str_detect(tolower(z), "capeox")])
    )
  )
    



dft_nccn_sum <- select(
  dft_nccn,
  date, file, active_year, 
  alpha_char_count, word_count, string_matches, 
  n_text_matches,
  n_matches_capeox
)

readr::write_rds(
  dft_nccn_sum,
  here('data', 'guideline_parse', 'nccn_rectal_cape_sum.rds')
)

