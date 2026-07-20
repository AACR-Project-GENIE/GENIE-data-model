library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_age_vars <- readr::read_rds(
    here('data', 'age_vars.rds')
)

dft_vio_cond <- bind_rows(
    mutate(age_vars, violation_type = "89_90"),
    mutate(age_vars, violation_type = "90_plus")
)

days_per_year = 365.25
# Note:  Months per year might seem odd.  But this has to be the case if we have
#   365.25 days per year and 30.4 days per month.  Empirically this seems to allow
#   us to backtrace what the stats team did, so here we are.
months_per_year = 12.0148

# In the future it would be better to store the conditions in a table and join.
# With only two types of conditions this is probably OK for today.
dft_vio_cond %<>%
    mutate(
        lb = case_when(
            unit %in% "day" & violation_type %in% "89_90" ~ 89 * days_per_year,
            unit %in% "month" & violation_type %in% "89_90" ~ 89 * months_per_year,
            unit %in% "year" & violation_type %in% "89_90" ~ 89,
            
            unit %in% "day" & violation_type %in% "90_plus" ~ 90 * days_per_year,
            unit %in% "month" & violation_type %in% "90_plus" ~ 90 * months_per_year,
            unit %in% "year" & violation_type %in% "90_plus" ~ 90,
            T ~ NA_real_,
        ),
        ub = case_when(
            unit %in% "day" & violation_type %in% "89_90" ~ 90 * days_per_year,
            unit %in% "month" & violation_type %in% "89_90" ~ 90 * months_per_year,
            unit %in% "year" & violation_type %in% "89_90" ~ 90,
            
            violation_type %in% "90_plus" ~ Inf,
            T ~ NA_real_
        )
    )

if ((dft_vio_cond %>% filter(is.na(lb) | is.na(ub)) %>% nrow) > 0) {
    cli_abort("Error in assigning bounds to violation conditions")
}

readr::write_rds(
    dft_vio_cond,
    here('data', 'vio_cond.rds')
)
            
            
            
            
            