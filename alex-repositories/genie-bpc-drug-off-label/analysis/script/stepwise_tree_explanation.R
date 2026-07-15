# Creates a D3 collapsible tree to show how we're lumping test categories.

library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

dft_stepwise_new_olu <- readr::read_rds(
  here('data', 'linked_approvals', 'stepwise', 'stepwise_new_off_label.rds')
)

dft_hdrug_determinations <- readr::read_rds(
  here('data', 'linked_approvals', 'hdrug_determinations.rds')
)

dft_step_explain <- dft_stepwise_new_olu %>%
  select(step) %>%
  mutate(step = as.character(step)) %>%
  filter(!(step %in% "<raw data>"))

dft_step_explain %<>%
  mutate(
    check_type = case_when(
      step %in% "test_met" ~ "Time-specific patient status",
      str_detect(step, "_biom_") ~ "Biomarker requirements",
      str_detect(step, "_with_") ~ "ConMed requirements",
      step %in% "test_ind_exists" ~ "Matching indication exists",
      step %in% "test_date_definite" ~ "Exposure after approval"
    ),
    failure_cat = case_when(
      step %in% "test_ind_exists" ~ "No indications found",
      step %in% "test_date_definite" ~ "Started before approval",
      T ~ "Specific req. not met"
    )
  )

chk_step_explain <- identical(
  sort(unique(dft_step_explain$failure_cat)),
  as.character(sort(unique(dft_hdrug_determinations$failure_type_f)))
)

if (!chk_step_explain) {
  cli_abort("Failure type names don't match - fix before you confuse someone!")
}

# Output a template table of tests done for the supplement.
dft_step_explain %>%
  select(check_type, step) %>%
  mutate(details = '') %>%
  flextable(.) %>%
  flextable::theme_box() %>%
  merge_v(j = 1) %>%
  valign(j = 1, valign = 'top') %>%
  padding(padding.top = 1, padding.bottom = 1) %>%
  flextable::fontsize(size = 10) %>%
  autofit(.) %>%
  flextable::align(part = 'header', align = 'left') %>%
  flextable::width(j = 1, width = 1.5) %>%
  flextable::width(j = 2, width = 2) %>%
  flextable::width(j = 3, width = 3) %>%
  flextable::save_as_docx(
    path = here('output', 'manu', 'supp_tables', 'test_details_template.docx')
  )

tree_pal <- c('white', '#eb5c68', 'white', '#4ef9cb')
tree_pal_vec <- c(
  tree_pal[1], # root
  rep(
    tree_pal[2],
    times = length(unique(pull(dft_step_explain, 'failure_cat')))
  ),
  rep(
    tree_pal[3],
    times = length(unique(pull(dft_step_explain, 'check_type')))
  ),
  rep(tree_pal[4], times = length(unique(pull(dft_step_explain, 'step'))))
)

ct_explain_tree <- collapsibleTree(
  dft_step_explain,
  hierarchy = c("failure_cat", "check_type", "step"),
  root = "Off label checks",
  collapsed = FALSE,
  zoomable = FALSE,
  fill = tree_pal_vec
)

readr::write_rds(
  ct_explain_tree,
  file = here('data', 'linked_approvals', 'js_tree_olu_cat.rds')
)
