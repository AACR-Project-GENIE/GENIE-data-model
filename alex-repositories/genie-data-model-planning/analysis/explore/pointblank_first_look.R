library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

curated_path <- here('data-raw', 'bpc', 'step1-curated', 'NSCLC2')

dat_ex <- readr::read_csv(
  dir_ls(here(curated_path, "DFCI")),
  # read everything as a character at this stage:
  col_types = cols(.default = col_character())
)

dat_dict <- readr::read_rds(
  here('data', 'bpc', 'step1-curated', 'aligned_data_dictionary.rds')
)


library(pointblank)

# I think the best way to figure out what workflow will suit me is programming some actual checks in:
req_cols <- dat_dict %>%
  filter(required) %>%
  pull(field_name)

comp_chk_cols <- dat_dict %>%
  filter(field_type %in% "complete_check") %>%
  pull(field_name)

# I'm going to go ahead and call those required too:
req_cols <- c(req_cols, comp_chk_cols)


# hydra = the abomination merged data we start with.
hydra_agent <- dat_ex %>%
  # select(-c(pt_start_time, pt_stop_time, redcap_ca_seq)) %>%
  create_agent(actions = action_levels(stop_at = 1))

hydra_agent %<>%
  col_exists(columns = all_of(req_cols))

hydra_agent %<>%
  rows_distinct(
    columns = c(
      'record_id',
      'redcap_repeat_instrument',
      'redcap_repeat_instance'
    )
  )

hydra_agent %<>%
  col_vals_in_set(
    columns = comp_chk_cols,
    set = c("2", NA),
    label = "All forms marked complete"
  )

hydra_intel <- hydra_agent %>%
  interrogate()

all_passed(hydra_intel)

get_agent_report(hydra_intel, display_table = TRUE) # a gt table

get_agent_report(hydra_intel, display_table = FALSE) # a tibble.

cli::cli_abort('stuff below this needs to go later now.')


#
#
#
#
#
#

# OK - so this takes a really long time to run due to the absolutely insane way the data is structured.  I think we're going to have to split the data, toss the trash, and then check the column values for this to run quickly.

valid_val_numeric_code_field_sets <- dat_dict %>%
  filter(!is.na(valid_val_str)) %>%
  group_by(valid_val_key_code) %>%
  summarize(field_names = list(c(field_name)))


for (i in 1:nrow(valid_val_numeric_code_field_sets)) {
  hydra_agent %<>%
    col_vals_in_set(
      columns = all_of(
        valid_val_numeric_code_field_sets[[i, "field_names"]][[1]]
      ),
      set = valid_val_numeric_code_field_sets[[i, "valid_val_key_code"]][[1]]
    )
}

hydra_agent %>%
  interrogate()
