
#' @description Big wrapper that fits the survival,
#'   returns the datasets. Because the model is really specific to this one use
#'   case, we didn't put this wrapper into the /R directory.
#' @param dat A dataset with one row per {record_id}.
#' @param boot_rep The number of bootstrap resamples to do.
#' @param main_seed The seed used to draw the bootstrap seeds.
#' @param additional_features Any features to add to the X matrix.
surv_fit_dmet_wrap <- function(
    dat, 
    boot_rep, 
    main_seed, 
    gene_feature_suffix = c(
      "mut",
      "cna",
      "fus",
      "lof",
      "gof",
      "gene"
    ),
    additional_features = c(
      "age_dx_c", 
      "stage_dx_iv_num",
      "birth_year_c",
      "white",
      "hispanic")
    ) {
  
  gene_regex <- paste(
    paste0("_", gene_feature_suffix, "$"),
    collapse = "|"
  )
  
  dat_sorted <- dat %>%
    # just to get the output in readable order we do an alphabet sort first:
    select(order(colnames(dat))) %>%
    select(
      tt_cpt_dmet_yrs_pos,
      tt_os_dmet_yrs,
      os_dx_status,
      matches(gene_regex),
      all_of(additional_features)
    ) 
  
  set.seed(main_seed) # for drawing each boot seed below
  dft_rep <- tibble(boot_ind = 1:boot_rep) %>%
    mutate(boot_seed = sample.int(n = 10^7, size = n()))
  
  rtn <- dft_rep %>%
    mutate(
      fit = furrr::future_map(
        .x = boot_seed,
        .f = (function(b) {
          fit_boot_cv_ltrc_lasso_2(
            dat = dat_sorted,
            x_col = "tt_cpt_dmet_yrs_pos",
            y_col = "tt_os_dmet_yrs",
            event_col = "os_dx_status",
            boot_seed = b,
            ignore_cols = character(0)
          )
        })
      )
    )
  
  return(rtn)
}
