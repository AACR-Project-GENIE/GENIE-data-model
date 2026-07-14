
library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_all <- readr::read_rds(
    here('data', 'all_dat.rds')
)

get_var_dat <- function(dat) {
    tibble(
        var = names(dat),
        type = purrr::map_chr(dat, typeof)
    )
}

# dft_all %>%
#     filter(dat_name %in% 'ca_ind') %>%
#     filter(cohort %in% "CRC") %>%
#     pull(dat) %>%
#     `[[`(.,1) %>%
#     get_var_dat

dft_all %<>%
    mutate(
        var_dat = map(
            .x = dat,
            .f = get_var_dat
        )
    )

dft_var <- dft_all %>%
    select(cohort, dat_name, var_dat) %>%
    unnest(var_dat)

dft_unique_var <- dft_var %>%
    group_by(var, type) %>%
    slice(1) %>%
    ungroup(.)

obvious_time_strings <- c(
    '_int_', '_age$', '^age', 
    '_days', '_mos', '_yrs'
)

dft_unique_var %>%
    filter(
        str_detect(
            var,
            paste(obvious_time_strings, collapse = "|")
        )
    ) %>%
    pull(var) %>% 
    unique
 

dft_age_vars <- readr::read_rds(here('data', 'age_vars.rds'))

dft_unique_var %>%
    filter(!(var %in% dft_age_vars$var)) %>%
    View(.)


dft_chelsea_age_var <- readr::read_csv(
    '/Users/apaynter/Downloads/age_variables_public.csv'
) %>%
    select(var = `Variable Name`)

setdiff(
    dft_chelsea_age_var$var,
    dft_age_vars$var
)

setdiff(
    dft_age_vars$var,
    dft_chelsea_age_var$var
)
