library(here)
library(fs)
library(purrr)

# hacky way to load all of our functions (no executed code):
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

source(here('analysis', 'script', 'get_raw_data.R'))
source(here('analysis', 'script', 'process_genomics.R'))
source(here('analysis', 'script', 'create_lines.R'))
source(here('analysis', 'script', 'drug_class_lists.R'))
source(here('analysis', 'script', 'build_cohort.R'))

# Render the HTML report:
quarto::quarto_render(
  input = here('analysis/report/cohort-build.qmd')
)

# I created a PDF report for this since a non-hosted HTML report (as in, sent in an email rather than put on a website) is not ideal.  This report is 100% redundant in content with the HTML, so I would ask tony which one he wants and delete the other.
quarto::quarto_render(
  input = here('analysis/report/cohort-build-pdf.qmd')
)
