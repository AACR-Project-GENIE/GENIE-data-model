# Note: This script is modified from one generously provided by Konrad Stopsack.
library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

pearl_groups <- readr::read_delim(
  here('data-raw', 'manual', 'dna_repair_genes_pearl.csv'),
  delim = ';'
)

pearl_groups %<>%
  mutate(
    pathway = case_when(
      str_detect(string = DDR, pattern = "Associated") & 
        str_detect(string = Process, pattern = "Checkpoint") ~
        "Checkpoint",
      str_detect(string = DDR, pattern = "Associated") ~
        "Other",
      str_detect(string = DDR, pattern = "probable") ~
        "Probable",
      TRUE ~ str_remove(
        string = Pathway1,
        pattern = "\\s*\\([^\\)]+\\)")
    )
  )

# As stated in Konrad's code the only gene missing a pathway now is H2AFZ.
pearl_groups %<>%
  filter(!is.na(pathway)) %>%
  select(pathway, gene = GeneID) %>% 
  distinct()

# This is the problem we're left with:
# pearl_groups %>% count(gene, sort = T) 

pearl_groups_distinct <- pearl_groups |>
  mutate(
    pathway = fct_relevel(
      factor(pathway),
      # priority of gene->pathway assignment:
      "MMR", "HR", "FA", "NHEJ", "BER", "NER")
  ) |>
  arrange(pathway) |>
  distinct(gene, .keep_all = TRUE)


# The rest of the code here I probably don't have a use for:
add_pathways <- function(data) {
  data |>
    select(any_of(genes_included)) |>
    mutate(id = row_number()) |>
    pivot_longer(
      cols = c(-id),
      names_to = "gene",
      values_to = "alt") |>
    left_join(
      pearl_groups_distinct, 
      by = "gene") |>
    group_by(id, pathway) |>
    summarize(
      alt = pmin(sum(alt, na.rm = TRUE), 1),  # cap at 1
      .groups = "drop") |>
    group_by(id, alt) |>
    arrange(pathway) |>
    mutate(
      dnar_pathway = if_else(
        alt > 0 & row_number() == 1, 
        true = pathway,
        false = factor(NA_integer_))) |>
    group_by(id) |>
    mutate(dnar_pathway = dnar_pathway[!is.na(dnar_pathway)][1]) |>
    ungroup() |>
    mutate(
      dnar_pathway = fct_relevel(
        fct_na_value_to_level(
          dnar_pathway,
          level = "None"),
        "None")) |>
    pivot_wider(
      names_from = pathway,
      values_from = alt) |>
    select(-id) %>%
    bind_cols(
      data, 
      .)
}
