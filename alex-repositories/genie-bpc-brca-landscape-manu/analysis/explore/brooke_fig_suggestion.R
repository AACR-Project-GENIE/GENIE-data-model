
n_gene <- 10

test_df <- tibble(
  gene = map_chr(seq_along(1:10), .f = \(x) {paste(sample(LETTERS, 3), collapse = "")})
) %>%
  slice(rep(1:n(), times = 3))

test_df %<>%
  mutate(
    site = rep(c("Right Earlobe", "Liver", "Left Foot"), each = n()/3),
    pts_alt = rnbinom(n = n(), size = 1, prob = 0.01),
    log_or = runif(n = n(), min = -3, max = 3),
    sig = if_else(abs(log_or) > 2, T, F)
  )

ggplot(
  test_df,
  aes(x = gene, y = 1, color = log_or, size = pts_alt)
) +
  geom_point(size = 15, color = "white", shape = 15) + 
  geom_point() + 
  facet_wrap(vars(site), ncol = 1) +
  theme_gray() + 
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text = element_text(hjust = 0)
  )
