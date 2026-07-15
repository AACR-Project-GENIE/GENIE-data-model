map_drug <- function(vec, drug_map, type = "class1") {
  vdf <- tibble(agent = vec)
  drug_map %<>% select(agent, out = all_of(type))
  vdf %<>% left_join(., drug_map, by = "agent")
  return(vdf$out)
}