#' Stability selection for Cox regression using glmnet
#'
#' @param x Predictor matrix
#' @param y Surv object
#' @param nsub Number of subsample splits.
#' @param frac Fraction of data per subsample (0.5 for complementary pairs)
#' @param cutoff Selection probability threshold (pi_thr)
#' @param PFER Desired per-family error rate bound (expected number of false selections)
#' @param q Max variables selected per subsample. If NULL, derived from PFER.
#' @param verbose Verbose = TRUE prints some messages to the console.
#' @param ... Additional arguments passed to \code{glmnet}
stabsel_glmnet_q_cap <- function(
  x,
  y,
  nsub = 100,
  frac = 0.5,
  cutoff = 0.75,
  PFER = 1,
  q = NULL,
  nfolds = 5,
  verbose = TRUE,
  ...
) {
  n <- nrow(x)
  p <- ncol(x)
  sub_size <- floor(n * frac)

  # Meinshausen & Buhlmann (2010), Theorem 1:
  #   E(PFER) <= q^2 / ((2*cutoff - 1) * p)
  # Solving for q:
  #   q = floor(sqrt(PFER * (2*cutoff - 1) * p))
  if (is.null(q)) {
    q <- floor(sqrt(PFER * (2 * cutoff - 1) * p))
    q <- max(1, min(q, p)) # q if q < p, 1 if q < 1, p otherwise.
  }

  # recompute the actual PFER bound given q, cutoff, p
  pfer_bound <- q^2 / ((2 * cutoff - 1) * p)

  if (verbose) {
    cli::cli_inform(c(
      "i" = "Settings: p = {p}, q = {q}, cutoff = {cutoff}",
      "i" = "PFER bound (E[false selections] <=): {round(pfer_bound, 3)}",
    ))
  }

  betas <- list()

  for (i in seq_len(nsub)) {
    perm <- sample(n)
    half1 <- perm[1:sub_size]
    half2 <- perm[(sub_size + 1):(2 * sub_size)]

    for (half in list(half1, half2)) {
      xs <- x[half, , drop = FALSE]
      ys <- y[half, ]

      beta_vec <- tryCatch(
        {
          # fit full path, pick lambda with <= q nonzero coefficients
          fit <- glmnet(xs, ys, family = "cox", ...)
          beta <- as.matrix(coef(fit))
          nvar <- colSums(beta != 0)
          valid <- which(nvar <= q)
          if (length(valid) == 0) {
            cli::cli_warn(
              "Even the most regularized version had > q (q={q}) vars.  This is unexpected behavior in glmnet."
            )
            idx <- 1
          } else {
            idx <- max(valid) # least regularised with <= q vars
          }
          beta[, idx]
        },
        error = function(e) NULL
      )

      if (!is.null(beta_vec)) {
        betas <- c(betas, list(beta_vec))
      }
    }

    if (i %% 10 == 0 & verbose) {
      cat(sprintf("Completed %d / %d subsample pairs\n", i, nsub))
    }
  }

  beta_at_q <- do.call(rbind, betas)
  colnames(beta_at_q) <- colnames(x)

  total_fits <- nrow(beta_at_q)
  sel_count <- colSums(beta_at_q != 0)
  sel_prob <- sel_count / total_fits
  stable_vars <- names(which(sel_prob >= cutoff))

  list(
    sel_prob = sel_prob,
    stable = stable_vars,
    beta_at_q = beta_at_q,
    cutoff = cutoff,
    q = q,
    pfer_bound = pfer_bound,
    nsub = nsub,
    total_fits = total_fits
  )
}
