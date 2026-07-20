library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

public_med_onc <- readr::read_csv(
  here('data-raw', 'NSCLC', '2.0-public', 'med_onc_note_level_dataset.csv')
)

p2_med_onc <- readr::read_csv(
  here('data-raw', 'NSCLC', '3.1-consortium', 'med_onc_note_level_dataset.csv')
)

public_cases_p2_data <- left_join(
  select(public_med_onc, record_id, md_visit_number),
  p2_med_onc,
  by = c('record_id', 'md_visit_number'),
  relationship = 'one-to-one'
)

# Do these have the same keys?
# waldo::compare(
#   select(public_cases_p2_data, record_id, md_onc_visit_int),
#   select(public_med_onc, record_id, md_onc_visit_int)
# )

anti_join(
  public_med_onc,
  p2_med_onc,
  by = c('record_id', 'md_onc_visit_int')
) %>%
  count(record_id) %>%
  print(n = 500)

anti_join(
  public_med_onc,
  p2_med_onc,
  by = c('record_id', 'md_visit_number')
) %>%
  count(record_id) %>%
  print(n = 500)

# Not great.  The set of md_onc_visit_int values in phase 1 is not released in phase 2, and the md visit numbers aren't there either.
# Let's have a look to see if they're all close to 89 years old:

cases_missing_in_phase_2 <- anti_join(
  public_med_onc,
  p2_med_onc,
  by = c('record_id', 'md_visit_number')
)

# Prompt: make a histogram of md_onc_visit_int var in cases_missing_in_phase_2 divided by 365.25 (in years)
# Prompt 2: make this look nicer.
# Prompt 3: I like the economist's graphs, make it look like that.
library(ggthemes)

ggplot(cases_missing_in_phase_2, aes(x = md_onc_visit_int / 365.25)) +
  geom_histogram(binwidth = 1, fill = "#3C78B5", color = "white") +
  labs(
    x = "Time from birth to oncology visit (years)",
    y = "Count",
    title = "Med onc visits in public release missing from phase 2",
    subtitle = paste0("n = ", nrow(cases_missing_in_phase_2), " visits")
  ) +
  theme_economist(base_size = 14) +
  scale_fill_fivethirtyeight() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text()
  )
