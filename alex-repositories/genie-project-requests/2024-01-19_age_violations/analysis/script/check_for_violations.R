library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_vio_cond <- readr::read_rds(
    here('data', 'vio_cond.rds')
)

dft_all <- readr::read_rds(
    here('data', 'all_dat.rds')
)

# function demos:

# test_bpc <- dft_all %>% slice(1) %>% pull(dat) %>% `[[`(.,1)

# find_violations(
#     test_bpc,
#     dft_vio_cond,
#     var_to_check = "dob_ca_dx_days"
# ) 

# find_all_violations(
#     test_bpc,
#     dft_vio_cond,
# ) 

dft_all %<>%
    mutate(
        viol = purrr::map(
            .x = dat,
            .f = \(x) {
                find_all_violations(
                    bpc_dat = x,
                    vio_dat = dft_vio_cond
                )
            }
        )
    )

dft_viol <- dft_all %>%
    select(cohort, dat_name, viol) %>%
    unnest(viol) 

dft_viol %>% View(.)

dft_viol %>% count(cohort, dat_name, violation_type)

dft_viol %>% count(cohort, var, violation_type) %>% View(.)

dft_viol %>% arrange(desc(violation_type), desc(.var_value)) %>% View

# Affected patients.
dft_viol %>% 
    count(cohort, violation_type, record_id) %>%
    count(cohort, violation_type) 

dft_viol %>%
    filter(violation_type %in% "90_plus")

# Special case:  Birth date.

dft_all %>%
    filter(dat_name %in% "pt") %>%
    mutate(
        bd_lt_1935 = purrr::map(
            .x = dat,
            .f = \(x) {
                select(x, record_id, birth_year) %>%
                    filter(birth_year < 1935)
            }
        )
    ) %>%
    select(cohort, bd_lt_1935) %>%
    unnest(bd_lt_1935)


dft_all %>%
    filter(str_detect(cohort, "NSCLC")) %>%
    mutate(
        has_ca_age = purrr::map(
            .x = dat,
            .f = \(x) "ca_age" %in% names(x)
        )
    ) %>%
    select(
        cohort, dat_name, has_ca_age
    ) %>%
    View(.)




# pts_with_violation <- dft_viol %>% 
#     count(violation_type, record_id) %>%
#     filter(violation_type %in% "90_plus") %>%
#     pull(record_id) %>%
#     unique
# 
# dft_all %>%
#     mutate(
#         contains_viol_pts = purrr::map_lgl(
#             .x = dat,
#             .f = \(x) {
#                 x %>%
#                     dplyr::filter(record_id %in% pts_with_violation) %>%
#                     nrow %>%
#                     magrittr::is_greater_than(.,1)
#             }
#         )
#     ) %>%
#     filter(contains_viol_pts) %>%
#     select(cohort, dat_name, contains_viol_pts)

# dft_all %>%
#     filter(str_detect(cohort, 'NSCLC_2.1')) %>%
#     filter(dat_name %in% 'ca_ind') %>%
#     pull(dat) %>%
#     `[[`(.,1) %>%
#     mutate(
#         problem_pt = record_id %in% pts_with_violation
#     ) %>%
#     ggplot(
#         dat = .,
#         aes(x = tt_os_dx_yrs, y = 1, color = problem_pt)
#     ) +
#     geom_jitter(height = 1, width = 0)
# 
# dft_all %>%
#     filter(str_detect(cohort, 'NSCLC_2.1')) %>%
#     filter(dat_name %in% 'ca_ind') %>%
#     pull(dat) %>%
#     `[[`(.,1) %>%
#     filter(record_id %in% pts_with_violation) %>%
#     pull(tt_os_dx_yrs)
#     

