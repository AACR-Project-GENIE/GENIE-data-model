
# Using my drugs dataset first just to get a rough idea on the right answers:
dft_abi_enza_doce <- dft_drug %>% 
  filter(str_detect(drug, "(Abiraterone|Enzalutamide|Docetaxel)")) %>%
  tabyl(drug)
  group_by(record_id, ca_seq, regimen_number) %>%
  summarize(
    reg_cat = case_when(
      any(str_detect(drug, "(Abira|Enza)")) & any(str_detect(drug, "Doce")) ~ "Abi/Enza, Docetaxel",
      any(str_detect(drug, "(Abira|Enza)")) ~ "Abi/Enza",
      any(str_detect(drug, "(Doce)")) ~ "Docetaxel",
      T ~ "Error"
    ),
    .groups = "drop"
  ) %>%
  mutate(reg_cat = factor(reg_cat)) %>%
  arrange(record_id, ca_seq, regimen_number) 

make_sunburst_input <- function(dat, var, order_var) {
  dat %>%
    arrange(.data[[order_var]]) %>%
    group_by(record_id, ca_seq) %>%
    summarize(
      path = paste(.data[[var]], collapse = "-"),
      .groups = "drop"
    ) %>%
    count(path) %>%
    arrange(path) %>%
    rename(Pop = n)
}

make_sunburst_input(
  dat = dft_abi_enza_doce,
  var = "reg_cat",
  order_var = "regimen_number"
)



  