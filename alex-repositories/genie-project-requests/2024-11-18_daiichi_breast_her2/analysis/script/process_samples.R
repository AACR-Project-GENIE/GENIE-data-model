library(here); library(purrr); library(fs)
purrr::walk(fs::dir_ls(here('R')), .f = source)

attrition_table <- readr::read_rds(
    here('data', 'bpc_attrition.rds')
)

attrition_table %<>%
    mutate(frac = n_bpc/first(n_bpc))

release <- readr::read_rds(
    here('data', 'release_sum_nested.rds')
)

release %<>%
    select(release = minor,
           dat) %>%
    unnest(dat)

# may need to be updated:
release %<>% 
    mutate(release = if_else(release %in% '17.5-consortium', '17.1-estimate', release))
    

center_region <- tribble(
    ~center, ~region,
    'COLU', 'US',
    'CRUK', 'Europe',
    'DFCI', 'US',
    'DUKE', 'US',
    'GRCC', 'Europe',
    'JHU', 'US',
    'MDA', 'US',
    'MSK', 'US',
    'NKI', 'Europe',
    'PROV', 'US',
    'SCI', 'Europe',
    'UCHI', 'US',
    'UCSF', 'US',
    'UHN', 'Canada',
    'VHIO', 'Europe',
    'VICC', 'US',
    'WAKE', 'US',
    'YALE', 'US'
)
    

release %<>%
    left_join(., center_region, by = "center") %>%
    relocate(region, .after = center)

if (any(is.na(release$region))) {
    cli_abort('Region is missing for at least one row.')
}

# rel_sum_site <- release %>%
#     group_by(release, region, center) %>%
#     summarize(n = length(unique(patient_id)), .groups = 'drop') %>%
#     filter(region %in% 'Europe') %>%
#     arrange(center, release)

rel_sum_n <- release %>%
    group_by(release, region) %>%
    summarize(n = length(unique(patient_id)), .groups = 'drop') 

rel_sum_n_tot <- release %>%
    group_by(release) %>%
    summarize(n = length(unique(patient_id)), .groups = 'drop') %>%
    mutate(region = "Total")

rel_sum_n <- bind_rows(
    rel_sum_n_tot,
    rel_sum_n
) %>%
    arrange(release, desc(n)) %>%
    mutate(region = fct_inorder(region))

readr::write_rds(
    rel_sum_n,
    here('data', 'release_summary_numbers.rds')
)

proj_n <- rel_sum_n %>%
    filter(release %in% '17.1-estimate') %>%
    select(-release) %>%
    pivot_wider(
        names_from = 'region',
        values_from = 'n'
    ) %>%
    mutate(step = first(attrition_table$step)) %>%
    left_join(attrition_table, ., by = 'step')

proj_n %<>%
    mutate(
        across(
            .cols = Total:Canada,
            .fns = \(z) case_when(
                # can change to nth if we want to genralize/function code this.
                is.na(z) ~ round(frac * first(z)),
                T ~ z
            )
        )
    )

readr::write_rds(
    proj_n,
    here('data', 'projections_past.rds')
)

get_slope <- function(x,y) {
    rtn <- lm(y ~ x) %>%
        broom::tidy(.) %>%
        filter(term %in% "x") %>%
        pull(estimate)
    return(rtn)
}

rel_sum_n %<>%
    mutate(
        yrs_from_11 = 
               (as.numeric(stringr::str_sub(release, 1, 2))-11)/2
    ) 

# get_slope(
#     filter(rel_sum_n, region %in% "Europe") %>% pull(yrs_from_11),
#     filter(rel_sum_n, region %in% "Europe") %>% pull(n)
# )
#     

rel_sum_accrual <- rel_sum_n %>%
    group_by(region) %>%
    summarize(
        slope = get_slope(yrs_from_11, n),
        .groups = 'drop'
    ) %>%
    mutate(slope = round(slope, 1))


proj_acc <- rel_sum_accrual %>%
    pivot_wider(
        names_from = 'region',
        values_from = 'slope'
    ) %>%
    mutate(step = first(attrition_table$step)) %>%
    left_join(attrition_table, ., by = 'step')

proj_acc %<>%
    mutate(
        across(
            .cols = Total:Canada,
            .fns = \(z) case_when(
                # can change to nth if we want to genralize/function code this.
                is.na(z) ~ round(frac * first(z)),
                T ~ z
            )
        )
    )

readr::write_rds(
    proj_acc,
    here('data', 'projections_acc.rds')
)


