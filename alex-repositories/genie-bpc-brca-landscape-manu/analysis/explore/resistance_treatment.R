# Writing this on Mar 1, 2024.  Evan asked a question about the "treatment analysis"
#   which to me has nebulous aims.  I'm wondering how many people have a NGS
#   test both before and after treatment with IO (or other drug classes).

# Sidenote: This is a really nice, quick way to check code in the plot below.

library(here); library(fs); library(purrr);
purrr::walk(.x = here("R", dir(here("R"))), .f = source)

load(here("data", "prog_cohorts.Rda")) # terrible programming, but this loads
#  the drug map.

# Writing to share with Evan:
readr::write_csv(
  dft_drug_map,
  here('analysis', 'explore', 'drug_map.csv'),
  na = ''
)

agent_interest <- dft_drug_map %>%
  # filter(class_comp %in% "CDK inhibitor") %>%
  # filter(class_comp %in% "IC inhibitor") %>%
  # filter(class_comp %in% "antiher2") %>%
  # filter(class_comp %in% "antivegf") %>%
  # filter(class_comp %in% "chemo") %>%
  # filter(class_comp %in% "endocrine") %>%
  # filter(class_comp %in% "other targeted") %>%
  # filter(class_comp %in% "parpi") %>%
  filter(class_comp %in% "pi3k pathway inhibitor") %>%
  pull(agent)

dft_drug_map %>% tabyl(class_comp)

dft_reg <- read_rds(here('data', 'clin_data_cohort', 'dft_reg.rds'))
    
dft_reg_interest <- dft_reg %>% 
  as_tibble(.) %>%
  filter(str_detect(regimen_drugs, paste(agent_interest, collapse = "|"))) %>%
  select(record_id, ca_seq, regimen_drugs, dx_reg_start_int) %>%
  mutate(event = "agent_start") %>%
  rename(dx_to_event = dx_reg_start_int)

dft_cpt <- read_rds(here('data', 'clin_data_cohort', 'dft_cpt.rds'))

# Just curious on this:
dft_cpt %>% 
  select(ca_seq, cpt_seq_date, dx_path_proc_cpt_days, dx_cpt_rep_days) %>%
  mutate(diff = dx_cpt_rep_days - dx_path_proc_cpt_days) %>%
  group_by(cpt_seq_date) %>% # also tried ca_seq as an explanation.
  summarize(
    diff = mean(diff,na.rm = T)
  )
# oh wow.  wowwwwwwwwwww.

dft_cpt %>%
  mutate(path_date_miss = is.na(dx_path_proc_cpt_days)) %>%
  tabyl(path_date_miss)
# mmmmk, no downside to using path dates then.

dft_cpt_interest <- dft_cpt %>%
  select(record_id, ca_seq, dx_to_event = dx_path_proc_cpt_days) %>%
  mutate(event = "ngs_path_proc") %>%
  left_join(
    distinct(select(dft_reg_interest, record_id, ca_seq)),
    .,
    by = c("record_id", "ca_seq")
  )

dft_interest <- bind_rows(
  dft_cpt_interest,
  dft_reg_interest
)

dft_interest %<>%
  arrange(record_id, ca_seq, dx_to_event)

dft_interest %<>%
  group_by(record_id, ca_seq) %>%
  mutate(
    this_is_ngs = event %in% "ngs_path_proc",
    prev_is_ngs = lag(this_is_ngs),
    this_is_agent = event %in% "agent_start",
    prev_is_agent = lag(this_is_agent),
    agent_after_ngs = this_is_agent & prev_is_ngs,
    ngs_after_agent_after_ngs = this_is_ngs & cumsum(replace_na(agent_after_ngs, F) >= 1),
  ) %>%
  mutate(
    across(
      .col = c(agent_after_ngs, ngs_after_agent_after_ngs),
      .fns = \(x) replace_na(x, F)
    )
  )

# just for plotting, still grouped:
dft_interest %<>%
  arrange(dx_to_event) %>%
  mutate(
    event_order = -(1:n()), # negative for plot
    subject_qual = any(ngs_after_agent_after_ngs)
  )

# still grouped
dft_sum <- dft_interest %>%
  summarize(
    # triple = ngs, agent, ngs sequence.
    has_triple = any(ngs_after_agent_after_ngs),
    .groups = "drop"
  )

dft_sum %>% filter(has_triple)

cli_abort("oh no a stop")


plot_random()  
plot_random()  
plot_random()  
plot_random()  
plot_random()  
plot_random()  # dft_sample <- dft_sum %>%
#   select(record_id, ca_seq) %>%
#   slice(sample(1:n(), 20)) %>%
#   left_join(
#     ., 
#     dft_interest,
#     by = c('record_id', 'ca_seq')
#   ) %>%
#   mutate(key = paste0(record_id, ca_seq))
#     
# 
# ggplot(
#   dft_sample,
#   aes(x = event, y = event_order, color = subject_qual)
# ) + 
#   geom_point(size = 2) + 
#   theme_bw() + 
#   facet_wrap(
#     vars(key),
#     ncol = 10
#   )

# trash code, makes namespace assumptions, sorry
plot_random <- function(my_seed = NULL) {
  if (is.null(my_seed)) {
    my_seed <- sample.int(10^5, 1)
    cli::cli_inform("Seed is {my_seed}.")
    set.seed(my_seed)
  }
  
  set.seed(my_seed)

  dft_sample <- dft_sum %>%
    select(record_id, ca_seq) %>%
    slice(sample(1:n(), 40)) %>%
    left_join(
      ., 
      dft_interest,
      by = c('record_id', 'ca_seq')
    ) %>%
    mutate(key = paste0(record_id, ';', ca_seq))
  
  ggplot(
    dft_sample,
    aes(y = event, x = event_order, color = subject_qual, group = key)
  ) + 
    geom_line(size = 1) + 
    theme_bw() + 
    facet_wrap(
      vars(key),
      ncol = 5
    ) + 
    theme(
      strip.text.x = element_text(size = 10, hjust = 0)
    )
}

plot_random(my_seed = 80954)
    


  