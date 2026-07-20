# Old FDA labels are useless garbage.  In order for our analyses to make sense,
#   we are considering excluding old drugs.  How old?  Not sure.  But first we 
#   need to find old drugs based on the indications sheet.

# This is somewhat outdated now with the old_drugs.qmd exploratory notebook
#   replacing it.

library(fs); library(purrr); library(here);
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

dft_ind_mapped <- readr::read_rds(
  here('data', 'warner_materials', 'indications_mapped.rds')
)

# Checking that they all match the pattern I hope they do:
dft_ind_mapped %>%
  filter(!str_detect(date, "[0-9]{4}-[0-9]{2}-[0-9]{2}") | is.na(date)) %>%
  tabyl(date)

dft_first_approvals <- dft_ind_mapped |> 
  filter(!is.na(date) & !is.na(mapped_agent)) |> 
  mutate(date = ymd(date)) |>
  group_by(mapped_agent) |>
  summarize(
    first_any_date = min(date, na.rm = T),
    first_fda_date = min(date[regulator == "FDA"], na.rm = T),
    num_rows_fda = sum(regulator == "FDA", na.rm = T),
    .groups = "drop"
  ) |> 
  arrange(first_date)

dft_first_approvals %>%
  filter(first_date != first_fda_date)

dft_first_approvals %>%
  filter(num_rows_fda %in% 0)

dft_first_approvals %>% View(.)

# We can work backwards on this a bit.  What are the drugs that have an absurdly
#   high number of violations for first uses in our initial analysis?
#   A verbatim copy is in this vector:
vec_heavy_violators <- c(
  "Leucovorin Calcium",
  "Oxaliplatin",
  "Carboplatin",
  "Irinotecan Hydrochloride",
  "Cisplatin",
  "Gemcitabine Hydrochloride",
  "Leuprolide Acetate",
  "Etoposide",
  "Methotrexate",
  # < 20 violations:
  "Paclitaxel",
  "Goserlin Acetate",
  "Mitomycin",
  "Vinblastine Sulfate",
  # And a few that pop up tons of times in the "anytime use" list:
  "Vinorelbine Tartrate",
  "Trastuzumab Emtansine",
  "Pegylated Liposomal Doxorubicin Hydrochloride",
  # < 100 violations:
  "Docetaxel"
)

dft_first_approvals %>%
  filter(mapped_agent %in% vec_heavy_violators) %>%
  arrange(first_fda_date)

ggplot(
  dft_first_approvals,
  aes(x = first_date, y = num_rows_fda)
) +
  geom_jitter(height = 0.25, width = 0)

dft_ind_mapped %>%
  filter(mapped_agent %in% "Irinotecan Hydrochloride") %>%
  filter(regulator %in% "FDA") %>%
  glimpse

