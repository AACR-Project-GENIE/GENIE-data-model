# Packages which seem to be available:
#   - stabs: not sure this has survival built in.
#   - c060: not sure this has truncation built in.
library(fs)
library(here)
library(purrr)
purrr::walk(.x = fs::dir_ls('R'), .f = source)

data("bodyfat", package = "TH.data")
set.seed(1234)

stab.lasso.glmnet <- stabs::stabsel(
  x = bodyfat[, -2],
  y = bodyfat[, 2],
  fitfun = stabs::glmnet.lasso,
  cutoff = 0.75,
  PFER = 1
)
# you can print the fit functions:
# glmnet.lasso
plot(stab.lasso.glmnet)


n_id <- 500
n_junk_var <- 100
easy_covar <- expand_grid(
  id = paste0('id', str_pad(1:n_id, side = 'left', pad = 0, width = 3)),
  var = c(
    'a1',
    'a2',
    paste0(
      'x',
      str_pad(1:n_junk_var, width = 3, side = 'left', pad = 0)
    )
  )
) %>%
  mutate(value = rbinom(n(), size = 1, prob = 0.3)) %>%
  pivot_wider(
    names_from = 'var',
    values_from = 'value'
  ) %>%
  select(-id) # added again below.

# a1 and a2 have strong effects, betas have none.
easy_beta <- c(-1, 1, rep(0, times = n_junk_var))
names(easy_beta) <- colnames(easy_covar)

easy_test <- gen_data_one(
  dat = easy_covar,
  beta = easy_beta,
  # surv shape and scale copied from sims.
  surv_shape = 0.7,
  surv_scale = 0.3,
  # truncation shape gives zero variance, scale gives us tiny trunc times.
  # that way everyone is observed.
  trunc_shape = 100,
  trunc_scale = 1000,
  censor_min = 4,
  censor_max = 15,
  limit_obs_n = NULL,
  return_type = "observed_combined",
  seed = 232
)


library(ggsurvfit)
fit <- survfit(Surv(time = x, time2 = y, event = event) ~ a2, data = easy_test)
ggsurvfit(fit) +
  add_risktable()


x_mat <- as.matrix(easy_test[, 5:ncol(easy_test)])

# Awesome - this finds them, but the coefficients are obviously a bit muted from the truth to say the least.
coef(cv.glmnet(
  x = x_mat,
  y = with(easy_test, Surv(time = x, time2 = y, event = event)),
  family = 'cox'
))

res_opt <- stabsel_glmnet_q_cap(
  x_mat,
  with(easy_test, Surv(time = x, time2 = y, event = event)),
  nsub = 100,
  cutoff = 0.75,
  PFER = 1,
  lambda_method = "q_cap"
)
mean_beta <- res_opt$beta_at_q %>% colMeans(.)
names(mean_beta) <- colnames(x_mat)

tibble(name = names(mean_beta), value = mean_beta) %>%
  ggplot(aes(x = value, y = reorder(name, value))) +
  geom_col() +
  labs(y = NULL)
mean_beta

plot_stabsel_cox(res_opt)

# Todo: return coefs from the model doing this.
# cut it up to work with my sims.
