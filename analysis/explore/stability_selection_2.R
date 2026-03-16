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
n_junk_var <- 3
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
fit <- survfit(Surv(time = x, time2 = y, event = event) ~ a1, data = easy_test)
ggsurvfit(fit) +
  add_risktable()


x_mat <- as.matrix(easy_test[, 5:ncol(easy_test)])

# Awesome - this finds them, but the coefficients are obviously a bit muted from the truth to say the least.
coef(cv.glmnet(
  x = x_mat,
  y = with(easy_test, Surv(time = x, time2 = y, event = event)),
  family = 'cox'
))

###########
# AI test #
###########

# This is written by AI:
stabsel_cox <- function(
  x,
  y,
  nsub = 100,
  frac = 0.5,
  cutoff = 0.75,
  nlambda = 100,
  ...
) {
  # x: predictor matrix
  # y: Surv object
  # nsub: number of subsamples
  # frac: fraction of data per subsample
  # cutoff: selection probability threshold

  n <- nrow(x)
  p <- ncol(x)
  sub_size <- floor(n * frac)

  # matrix to track how often each variable is selected at each lambda
  sel_prob <- matrix(0, nrow = p, ncol = 1)
  rownames(sel_prob) <- colnames(x)

  # for complementary pairs (Shah & Samworth 2013), draw n/2 each time
  for (i in seq_len(nsub)) {
    print('meh')
    idx <- sample(n, sub_size, replace = FALSE)

    x_sub <- x[idx, , drop = FALSE]
    y_sub <- y[idx, ]

    # fit lasso on subsample
    fit <- tryCatch(
      glmnet(
        x_sub,
        y_sub,
        family = "cox",
        nlambda = nlambda,
        ...
      ),
      error = function(e) {
        print('errored out')
        NULL
      }
    )
    if (is.null(fit)) {
      next
    }

    beta <- as.matrix(coef(fit))
    # for each variable, was it ever selected along the path?
    # more useful: was it selected at each lambda?
    # standard approach: count if selected at *any* lambda
    ever_selected <- rowSums(beta != 0) > 0
    sel_prob[, 1] <- sel_prob[, 1] + ever_selected
  }

  sel_prob <- sel_prob / nsub

  # variables whose selection probability exceeds cutoff
  stable_vars <- names(which(sel_prob[, 1] >= cutoff))

  list(
    sel_prob = sel_prob[, 1],
    stable = stable_vars,
    cutoff = cutoff,
    nsub = nsub
  )
}

stabsel_cox(
  x = x_mat,
  y = with(easy_test, Surv(time = x, time2 = y, event = event)),
  family = 'cox',
  nsub = 1
)

glmnet(
  x = x_mat[1:100, , drop = FALSE],
  y = with(easy_test, Surv(time = x, time2 = y, event = event))[1:100, ],
  family = 'cox'
)
