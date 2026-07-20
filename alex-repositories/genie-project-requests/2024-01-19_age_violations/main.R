# The top-level file for investigating HIPAA-type age violations in GENIE BPC.

# Note:  This currently only looks at zero-calculation issues, meaning variables
#   that literally state an age outside of the accepted ranges.  For example,
#   we do not look at the difference betweeen sequencing and birth year even
#   though that's an absurdly obvious way to infer a person's age based on the 
#   data.

library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

# Workflow:
source(here('analysis', 'script', 'create_folders.R'))
source(here('analysis', 'script', 'get_raw_data.R'))
source(here('analysis', 'script', 'declare_age_vars.R'))

# Optional - needs to be manually opened and run line by line:
# here('analysis', 'explore', 'explore_age_vars_build_upon.R')

# This needs to be run line by line probably, but here's the file:
source(here('analysis', 'script', 'check_for_violations.R'))

