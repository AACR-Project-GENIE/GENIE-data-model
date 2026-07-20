library(synapser)
synLogin()

get_synapse_entity_data_in_csv <- function(
  synapse_id,
  version = NA,
  na.strings = c("", "NA"),
  comment.char = "#"
) {
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }

  data <- readr::read_csv(
    entity$path,
    na = na.strings,
    comment = comment.char
  )
  return(data)
}

# Just as an example, MSK NSCLC2 data
redcap_ex <- get_synapse_entity_data_in_csv(
  'syn27351615'
)

redcap_ex <- redcap_ex %>%
  arrange(redcap_repeat_instance, record_id)

redcap_mat <- redcap_ex %>% as.matrix()
redcap_mat <- apply(redcap_mat, 2, FUN = \(x) as.numeric(is.na(x)))

redcap_long_na <- redcap_ex %>%
  mutate(row = 1:n()) %>%
  mutate(
    across(
      .cols = -row,
      .fns = \(x) as.numeric(is.na(x))
    )
  ) %>%
  pivot_longer(
    cols = -row,
    names_to = "var",
    values_to = 'missing'
  ) %>%
  mutate(
    var = forcats::fct_inorder(var)
  )


sparse_y_labs <- levels(redcap_long_na$var)
sparse_y_labs <- case_when(
  1:length(sparse_y_labs) %% 10 == 0 ~ sparse_y_labs,
  T ~ ""
)

library(ggtext)

gg <- ggplot(data = redcap_long_na, aes(x = row, y = var, z = missing)) +
  stat_summary_2d(bins = c(100, 100)) +
  scale_y_discrete(labels = sparse_y_labs) +
  scale_x_continuous(name = "Rows") +
  scale_fill_viridis_c(
    option = "magma",
    labels = c("Complete", "Missing"),
    breaks = c(0, 1),
    limits = c(0, 1)
  ) +
  labs(
    title = "Sparsity of the REDcap export",
    subtitle = "100 vertical and horizontal averaged bins <br> and only labelling every 10th variable <br> so my computer doesn't melt"
  ) +
  theme(title = element_markdown())

ggsave(
  gg,
  height = 10,
  width = 8,
  filename = here::here('analysis', 'explore', 'redcap_sparsity.png')
)
