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
easy_beta <- c(-0.5, 0.5, rep(0, times = n_junk_var))
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
    idx <- sample(n, sub_size, replace = FALSE)

    x_sub <- x[idx, , drop = FALSE]
    y_sub <- y[idx, ]

    glmnet(
      x_sub,
      y_sub,
      family = "cox",
      nlambda = nlambda,
      ...
    )

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
  nsub = 30
)

glmnet(
  x = x_mat[1:100, , drop = FALSE],
  y = with(easy_test, Surv(time = x, time2 = y, event = event))[1:100, ],
  family = 'cox'
)


stabsel_cox_2 <- function(
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
  # frac: fraction of data per subsample (0.5 for complementary pairs)
  # cutoff: selection probability threshold
  # nlambda: number of lambda values in the glmnet path

  n <- nrow(x)
  p <- ncol(x)
  sub_size <- floor(n * frac)

  # matrix to track selection probability at each lambda
  # rows = variables, columns will accumulate counts
  sel_count_any <- rep(0, p) # ever selected along the path
  sel_count_by_lambda <- NULL # per-lambda tracking (initialised on first fit)
  names(sel_count_any) <- colnames(x)
  total_fits <- 0

  for (i in seq_len(nsub)) {
    perm <- sample(n)
    half1 <- perm[1:sub_size]
    half2 <- perm[(sub_size + 1):(2 * sub_size)]

    for (half in list(half1, half2)) {
      fit <- tryCatch(
        glmnet(
          x[half, , drop = FALSE],
          y[half, ],
          family = "cox",
          nlambda = nlambda,
          ...
        ),
        error = function(e) NULL
      )
      if (is.null(fit)) {
        next
      }

      beta <- as.matrix(coef(fit))
      selected_mat <- (beta != 0) * 1

      # per-lambda tracking: accumulate selection indicators
      # glmnet may return different numbers of lambdas across fits,
      # so we track the *maximum* selection probability across the path
      max_selected <- apply(selected_mat, 1, max)
      sel_count_any <- sel_count_any + max_selected

      # also store the full per-lambda selection matrix for curves
      if (is.null(sel_count_by_lambda)) {
        sel_count_by_lambda <- selected_mat
      } else {
        # align to common number of lambdas (use the smaller)
        ncol_common <- min(ncol(sel_count_by_lambda), ncol(selected_mat))
        sel_count_by_lambda <- sel_count_by_lambda[,
          1:ncol_common,
          drop = FALSE
        ] +
          selected_mat[, 1:ncol_common, drop = FALSE]
      }

      total_fits <- total_fits + 1
    }
  }

  # selection probabilities
  sel_prob <- sel_count_any / total_fits
  names(sel_prob) <- colnames(x)

  # per-lambda selection probability curves
  sel_prob_by_lambda <- sel_count_by_lambda / total_fits
  rownames(sel_prob_by_lambda) <- colnames(x)

  # stable variables
  stable_vars <- names(which(sel_prob >= cutoff))

  list(
    sel_prob = sel_prob,
    sel_prob_by_lambda = sel_prob_by_lambda,
    stable = stable_vars,
    cutoff = cutoff,
    nsub = nsub,
    total_fits = total_fits
  )
}

res <- stabsel_cox_2(
  x = x_mat,
  y = with(easy_test, Surv(time = x, time2 = y, event = event)),
  nsub = 30
)

plot_stabsel_cox <- function(result, top_n = NULL) {
  mat <- result$sel_prob_by_lambda
  if (!is.null(top_n)) {
    top_vars <- names(sort(result$sel_prob, decreasing = TRUE))[
      1:min(top_n, nrow(mat))
    ]
    mat <- mat[top_vars, , drop = FALSE]
  }

  cols <- rainbow(nrow(mat))
  plot(
    NULL,
    xlim = c(1, ncol(mat)),
    ylim = c(0, 1),
    xlab = "Lambda index (most to least regularised)",
    ylab = "Selection probability",
    main = "Stability selection paths"
  )
  abline(h = result$cutoff, lty = 2, col = "grey40")
  for (j in seq_len(nrow(mat))) {
    lines(seq_len(ncol(mat)), mat[j, ], col = cols[j], lwd = 1.5)
  }
  legend(
    "topleft",
    legend = rownames(mat),
    col = cols,
    lwd = 1.5,
    cex = 0.7,
    ncol = 2,
    bg = "white"
  )
}

plot_stabsel_cox(res)


library(glmnet)
library(survival)

stabsel_cox_3 <- function(
  x,
  y,
  nsub = 100,
  frac = 0.5,
  cutoff = 0.75,
  lambda_rule = "lambda.1se",
  nfolds = 5,
  ...
) {
  # x: predictor matrix
  # y: Surv object
  # nsub: number of subsamples
  # frac: fraction of data per subsample (0.5 for complementary pairs)
  # cutoff: selection probability threshold
  # lambda_rule: "lambda.min" or "lambda.1se" from cv.glmnet
  # nfolds: number of CV folds within each subsample

  n <- nrow(x)
  p <- ncol(x)
  sub_size <- floor(n * frac)

  sel_count <- rep(0, p)
  names(sel_count) <- colnames(x)
  total_fits <- 0

  for (i in seq_len(nsub)) {
    perm <- sample(n)
    half1 <- perm[1:sub_size]
    half2 <- perm[(sub_size + 1):(2 * sub_size)]

    for (half in list(half1, half2)) {
      cvfit <- tryCatch(
        cv.glmnet(
          x[half, , drop = FALSE],
          y[half, ],
          family = "cox",
          nfolds = nfolds,
          ...
        ),
        error = function(e) NULL
      )
      if (is.null(cvfit)) {
        next
      }

      # extract coefficients at the CV-chosen lambda
      beta <- as.numeric(coef(cvfit, s = lambda_rule))
      sel_count <- sel_count + (beta != 0)
      total_fits <- total_fits + 1
    }

    # progress
    if (i %% 10 == 0) {
      cat(sprintf("Completed %d / %d subsample pairs\n", i, nsub))
    }
  }

  sel_prob <- sel_count / total_fits
  stable_vars <- names(which(sel_prob >= cutoff))

  list(
    sel_prob = sel_prob,
    stable = stable_vars,
    cutoff = cutoff,
    nsub = nsub,
    total_fits = total_fits
  )
}

# --- plotting helper ---
plot_stabsel_cox <- function(result) {
  probs <- sort(result$sel_prob, decreasing = TRUE)
  barplot(
    probs,
    las = 2,
    ylim = c(0, 1),
    ylab = "Selection probability",
    main = "Stability selection (Cox, CV lambda)",
    col = ifelse(probs >= result$cutoff, "steelblue", "grey70"),
    border = NA
  )
  abline(h = result$cutoff, lty = 2, col = "red", lwd = 1.5)
  legend(
    "topright",
    legend = c("Stable", "Not stable", paste0("Cutoff = ", result$cutoff)),
    fill = c("steelblue", "grey70", NA),
    border = NA,
    lty = c(NA, NA, 2),
    col = c(NA, NA, "red"),
    lwd = c(NA, NA, 1.5),
    bg = "white"
  )
}


res3 <- stabsel_cox_3(
  x = x_mat,
  y = with(easy_test, Surv(time = x, time2 = y, event = event)),
  nsub = 30
)

res_opt <- stabsel_cox_q_options(
  x_mat,
  with(easy_test, Surv(time = x, time2 = y, event = event)),
  nsub = 100,
  cutoff = 0.75,
  PFER = 1,
  lambda_method = "q_cap"
)
res_opt

plot_stabsel_cox(res_opt)

# Todo: return coefs from the model doing this.
# cut it up to work with my sims.
