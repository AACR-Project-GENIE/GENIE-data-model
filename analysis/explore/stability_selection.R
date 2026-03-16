# Packages which seem to be available:
#   - stabs: not sure this has survival built in.
#   - c060: not sure this has truncation built in.
library(fs)
library(here)
library(purrr)
purrr::walk(.x = fs::dir_ls('R'), .f = source)

library(stabs)
library(lars)
library(c060)
## make data set available
data("bodyfat", package = "TH.data")
set.seed(1234)

stab.lasso <- stabsel(
  x = bodyfat[, -2],
  y = bodyfat[, 2],
  fitfun = lars.lasso,
  cutoff = 0.75,
  PFER = 1
)

stab.lasso

stab.stepwise <- stabsel(
  x = bodyfat[, -2],
  y = bodyfat[, 2],
  fitfun = lars.stepwise,
  cutoff = 0.75,
  PFER = 1
)
stab.stepwise

par(mfrow = c(2, 1))
plot(stab.lasso, main = "Lasso")
plot(stab.stepwise, main = "Stepwise Selection")

stab.lasso.glmnet <- stabsel(
  x = bodyfat[, -2],
  y = bodyfat[, 2],
  fitfun = glmnet.lasso,
  cutoff = 0.75,
  PFER = 1
)
# you can print the fit functions:
# glmnet.lasso
plot(stab.lasso.glmnet)
plot(stab.lasso)

# Arguments: you MUST specify two of the following three:
# - cutoff: parameter cutoff, recommend between 0.6 and 0.9.
# - q:  The number of selected variables on each subsample.
# - PFER: upper bound for per-family error rate.  Number of falsely selected 'learners'.  They said PFER is more conservative so FWER is automatically controlled, nice.

# install.packages("devtools")
# devtools::install_github("fbertran/c060")

n_id <- 500
n_junk_var <- 100
easy_test <- expand_grid(
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
  mutate(value = rbinom(n(), size = 1, prob = 0.1)) %>%
  pivot_wider(
    names_from = 'var',
    values_from = 'value'
  ) %>%
  select(-id) # added again below.

easy_beta <- c(-0.5, 0.5, rep(0, times = n_junk_var))
names(easy_beta) <- colnames(easy_test)

easy_test <- gen_data_one(
  dat = easy_test,
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

y_mat <- as.matrix(easy_test[, 3:4])
colnames(y_mat) <- c('time', 'status') # required.
x_mat <- as.matrix(easy_test[, 5:ncol(easy_test)])

sp_test <- c060::stabpath(
  y = y_mat,
  x = x_mat,
  weakness = 1,
  family = 'cox'
)

plot(sp_test) # pi thresh and such can be modified here.

stabsel(sp_test, error = 0.05, type = 'pfer')
stabsel(sp_test, error = 0.5)
stabsel(sp_test, error = 0.9)
stabsel(sp_test, error = 5) # Ok, odd that it takes invalid probabilities here.
stabsel(sp_test, error = 0.99, type = 'pcer')
ss_test <- stabsel(sp_test, error = 0.05, type = 'pcer')
stabsel(sp_test, error = 0.05, type = 'pcer', pi = 0.501)
stabsel(sp_test, error = 0.1, type = 'pcer', pi = 0.9) %>% str

plot(sp_test, error = 0.99)

plot(
  x = stabsel(ss_test, error = 0.1)
)


stabsel(x, error = 0.05, type = c("pfer", "pcer"), pi_thr = 0.6)


# OK, gotta give this a try on the breast cancer data and see what shakes out:
dft_surv_all <- readr::read_rds(
  here('data', 'survival', 'prepared_data', 'surv_dmet_all.rds')
)

dft_surv_all


library("stabs")
library("mboost")
### low-dimensional example
mod <- glmboost(DEXfat ~ ., data = bodyfat)

## compute cutoff ahead of running stabsel to see if it is a sensible
## parameter choice.
##   p = ncol(bodyfat) - 1 (= Outcome) + 1 ( = Intercept)
stabsel_parameters(
  q = 3,
  PFER = 1,
  p = ncol(bodyfat) - 1 + 1,
  sampling.type = "MB"
)
## the same:
stabsel(mod, q = 3, PFER = 1, sampling.type = "MB", eval = FALSE)

## now run stability selection
(sbody <- stabsel(mod, q = 3, PFER = 1, sampling.type = "MB"))
opar <- par(mai = par("mai") * c(1, 1, 1, 2.7))
plot(sbody, type = "paths")
par(opar)

plot(sbody, type = "maxsel", ymargin = 6)
