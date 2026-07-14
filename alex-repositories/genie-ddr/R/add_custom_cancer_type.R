add_custom_cancer_type <- function(
  dat,
  min_n_per_cancer = 10
) {
  tumor_types <- mskcc.oncotree::get_tumor_types()
  dat %<>%
    left_join(
      .,
      select(tumor_types, oncotree_code, tissue),
      by = "oncotree_code"
    )

  dat %<>%
    mutate(
      custom_cancer_type = case_when(
        oncotree_code %in% c("UNKNOWN", "CUP", "CUPNOS") ~ "Unknown",
        # Three types for bladder:
        oncotree_code %in% "UTUC" ~ "Bladder (UTUC)",
        oncotree_code %in% "BLCA" ~ "Bladder (BLCA)",
        tissue %in% "Bladder/Urinary Tract" ~ "Bladder (other)",
        # For some reason glioma's dont have a tissue type...
        # My fix is to use the cancer type for those, as much as I hate that.
        is.na(tissue) ~ cancer_type,
        TRUE ~ tissue
      )
    )

  # We're losing some important groups doing this, but I want to see the plot:
  dat %<>%
    group_by(custom_cancer_type) %>%
    mutate(n_custom_cancer_type = n()) %>%
    ungroup(.) %>%
    filter(n_custom_cancer_type >= min_n_per_cancer)

  # Make a feature for cancer type with an n:
  dat %<>%
    group_by(custom_cancer_type) %>%
    mutate(n_ct = n()) %>%
    ungroup(.) %>%
    # putting these in order with weird sorting so I don't have to mess with the n's later on:
    arrange(
      desc(str_detect(custom_cancer_type, "BLCA")),
      desc(str_detect(custom_cancer_type, "UTUC")),
      desc(str_detect(custom_cancer_type, "Bladder \\(other\\)")),
      custom_cancer_type
    )

  dat %<>%
    mutate(
      # leaving the paren open here on purpose:
      custom_cancer_type = glue('{custom_cancer_type} ({n_ct}'),
      custom_cancer_type = fct_inorder(custom_cancer_type),
      custom_cancer_type = fct_rev(custom_cancer_type)
    )

  dat
}
