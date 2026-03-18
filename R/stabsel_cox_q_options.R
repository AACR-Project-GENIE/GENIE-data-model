library(glmnet)
library(survival)
#' Stability selection for Cox regression using glmnet
#'
#' @param x Predictor matrix
#' @param y Surv object
#' @param nsub Number of subsamples
#' @param frac Fraction of data per subsample (0.5 for complementary pairs)
#' @param cutoff Selection probability threshold (pi_thr)
#' @param PFER Desired per-family error rate bound (expected number of false selections)
#' @param q Max variables selected per subsample. If NULL, derived from PFER.
#' @param lambda_method One of \code{"q_cap"} (pick the lambda giving <= q nonzero
#'   coefficients, pure M&B), \code{"cv_cap"} (use cv.glmnet lambda, but cap at q
#'   variables if it exceeds), or \code{"cv_only"} (use cv.glmnet lambda with no cap,
#'   no formal PFER guarantee)
#' @param nfolds CV folds (only used if lambda_method involves cv)
#' @param ... Additional arguments passed to \code{glmnet}
stabsel_cox_q_options <- function(
  x,
  y,
  nsub = 100,
  frac = 0.5,
  cutoff = 0.75,
  PFER = 1,
  q = NULL,
  lambda_method = c("q_cap", "cv_cap", "cv_only"),
  nfolds = 5,
  ...
) {
  lambda_method <- match.arg(lambda_method)
  n <- nrow(x)
  p <- ncol(x)
  sub_size <- floor(n * frac)

  # Meinshausen & Buhlmann (2010), Theorem 1:
  #   E(V) <= q^2 / ((2*cutoff - 1) * p)
  # Solving for q:
  #   q = floor(sqrt(PFER * (2*cutoff - 1) * p))
  if (is.null(q)) {
    q <- floor(sqrt(PFER * (2 * cutoff - 1) * p))
    if (q < 1) {
      q <- 1
    }
    if (q > p) q <- p
  }

  # recompute the actual PFER bound given q, cutoff, p
  pfer_bound <- q^2 / ((2 * cutoff - 1) * p)

  cat(sprintf("Settings: p = %d, q = %d, cutoff = %.2f\n", p, q, cutoff))
  cat(sprintf("PFER bound (E[false selections] <=): %.3f\n", pfer_bound))
  cat(sprintf("Lambda method: %s\n\n", lambda_method))

  sel_count <- rep(0, p)
  names(sel_count) <- colnames(x)
  total_fits <- 0

  for (i in seq_len(nsub)) {
    perm <- sample(n)
    half1 <- perm[1:sub_size]
    half2 <- perm[(sub_size + 1):(2 * sub_size)]

    for (half in list(half1, half2)) {
      xs <- x[half, , drop = FALSE]
      ys <- y[half, ]

      selected <- tryCatch(
        {
          if (lambda_method == "q_cap") {
            # fit full path, pick lambda with <= q nonzero coefficients
            fit <- glmnet(xs, ys, family = "cox", ...)
            beta <- as.matrix(coef(fit))
            nvar <- colSums(beta != 0)
            valid <- which(nvar <= q)
            if (length(valid) == 0) {
              # even the most regularised solution has > q vars; take it anyway
              idx <- 1
            } else {
              idx <- max(valid) # least regularised with <= q vars
            }
            as.numeric(beta[, idx] != 0)
          } else if (lambda_method == "cv_cap") {
            # use cv.glmnet, but enforce q cap
            cvfit <- cv.glmnet(xs, ys, family = "cox", nfolds = nfolds, ...)
            beta_cv <- as.numeric(coef(cvfit, s = "lambda.1se"))
            n_selected <- sum(beta_cv != 0)
            if (n_selected <= q) {
              as.numeric(beta_cv != 0)
            } else {
              # cv selected too many; fall back to q cap on the full path
              fit <- cvfit$glmnet.fit
              beta <- as.matrix(coef(fit))
              nvar <- colSums(beta != 0)
              valid <- which(nvar <= q)
              idx <- if (length(valid) == 0) 1 else max(valid)
              as.numeric(beta[, idx] != 0)
            }
          } else {
            # cv_only — no cap, no PFER guarantee
            cvfit <- cv.glmnet(xs, ys, family = "cox", nfolds = nfolds, ...)
            beta_cv <- as.numeric(coef(cvfit, s = "lambda.1se"))
            as.numeric(beta_cv != 0)
          }
        },
        error = function(e) NULL
      )

      if (is.null(selected)) {
        next
      }

      sel_count <- sel_count + selected
      total_fits <- total_fits + 1
    }

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
    q = q,
    pfer_bound = pfer_bound,
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
    main = sprintf(
      "Stability selection (q=%d, PFER<=%.2f)",
      result$q,
      result$pfer_bound
    ),
    col = ifelse(probs >= result$cutoff, "steelblue", "grey70"),
    border = NA
  )
  abline(h = result$cutoff, lty = 2, col = "red", lwd = 1.5)
  legend(
    "topright",
    legend = c("Stable", "Not stable", sprintf("Cutoff = %.2f", result$cutoff)),
    fill = c("steelblue", "grey70", NA),
    border = NA,
    lty = c(NA, NA, 2),
    col = c(NA, NA, "red"),
    lwd = c(NA, NA, 1.5),
    bg = "white"
  )
}
