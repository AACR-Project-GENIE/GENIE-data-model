# Script to derive the first approval date(s) for each drug and save the list
#   of drugs that are "too old" to be considered.

library(fs); library(purrr); library(here);
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

dft_ind_mapped <- readr::read_rds(
  here('data', 'warner_materials', 'indications_mapped.rds')
)

dft_first_approvals <- dft_ind_mapped |> 
  filter(!is.na(date) & !is.na(mapped_agent)) |> 
  mutate(date = ymd(date)) |>
  group_by(mapped_agent) |>
  summarize(
    first_global_date = min(date, na.rm = T),
    first_fda_date = min(date[regulator == "FDA"], na.rm = T),
    indication_rows_fda = sum(regulator == "FDA", na.rm = T),
    .groups = "drop"
  ) |> 
  arrange(first_global_date)

write_rds(
  x = dft_first_approvals,
  file = here('data', 'warner_materials', 'first_approvals.rds')
)


vec_old_drugs <- dft_first_approvals %>%
  filter(first_global_date < ymd('1997-01-01')) %>%
  pull(mapped_agent)

write_rds(
  x = vec_old_drugs,
  file = here('data', 'warner_materials', 'old_drugs_list.rds')
)
