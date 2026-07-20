
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

# Technically this gets unique combinations of variable and type.
dft_unique_var <- dft_var %>%
    group_by(var, type) %>%
    slice(1) %>%
    ungroup(.)

obvious_time_strings <- c(
    '_int', '_age$', '^age', 
    '_days', '_mos', '_yrs'
)

dft_age_vars <- readr::read_rds(here('data', 'age_vars.rds'))

# A helper printout to discover variables that look to be 
# time related but are not yet declared in our file.
dft_unique_var %>%
    mutate(
        looks_like_time_string = str_detect(
            var,
            paste(obvious_time_strings, collapse = "|")
        ),
        not_in_declared_vars = !(var %in% dft_age_vars$var)
    ) %>%
    filter(
        looks_like_time_string & not_in_declared_vars
    ) %>%
    arrange(cohort) %>%
    View(.)
 
# A much greater burden:  look through all the variables to see if any appear
#   to be agey.
dft_unique_var %>%
    filter(!(var %in% dft_age_vars$var)) %>%
    View(.)








# Some sloppy checks for typos and errors in my age variable entry...

# Check that each variable you declared shows up in at least one dataset.
# This list should be empty if so:
dft_age_vars %>%
    filter(!(var %in% dft_unique_var$var))

get_var_dat_with_ranges <- function(dat, get_min_max_etc = F) {
    rtn <- tibble(
        var = names(dat),
        type = purrr::map_chr(dat, typeof),
        min = purrr::map_dbl(
            .x = dat,
            .f = \(x) {
                y <- min(x, na.rm = T)
                if (is.character(y)) return(NA) else return(y)
            }
        ),
        max = purrr::map_dbl(
            .x = dat,
            .f = \(x) {
                y <- max(x, na.rm = T)
                if (is.character(y)) return(NA) else return(y)
            }
        ),
        all_na = purrr::map_lgl(
            .x = dat,
            .f = \(x) all(is.na(x))
        )
    )
    
    return(rtn)
    
}

# Check that all the year variables are in the range [0,120)
dft_all %<>%
    mutate(
        var_dat = map(
            .x = dat,
            .f = get_var_dat_with_ranges
        )
    )

dft_var_2 <- dft_all %>%
    select(cohort, dat_name, var_dat) %>%
    unnest(var_dat)

dft_var_2 %<>%
    filter(!(type %in% 'character')) %>%
    filter(!all_na) %>%
    filter(var %in% dft_age_vars$var) %>%
    left_join(
        .,
        dft_age_vars,
        by = 'var'
    )
    
# We would expect no entries in this list.
dft_var_2 %>%
    filter(
        unit %in% "year" & max > 120,
        unit %in% "month" & max > 120*12,
        # Can't think of any day variables which wouldn't be over 120 once or twice:
        unit %in% "day" & max < 120
    )
    
