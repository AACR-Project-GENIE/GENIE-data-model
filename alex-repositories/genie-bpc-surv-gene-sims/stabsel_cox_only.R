stabsel_cox_only <- function(
  x,
  y,
  nsub = 100,
  frac = 0.5,
  cutoff = 0.75,
  PFER = 1,
  q = NULL,
  lambda_method = c("q_cap", "cv_cap", "cv_only"),
  nfolds = 5,
  verbose = TRUE,
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

  if (verbose) {
    cli::cli_inform(c(
      "i" = "Settings: p = {p}, q = {q}, cutoff = {cutoff}",
      "i" = "PFER bound (E[false selections] <=): {round(pfer_bound, 3)}",
      "i" = "Lambda method: {lambda_method}"
    ))
  }

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

      rtn <- tryCatch(
        {
          # fit full path, pick lambda with <= q nonzero coefficients
          fit <- glmnet(xs, ys, family = "cox", ...)
          beta <- as.matrix(coef(fit))
          nvar <- colSums(beta != 0)
          valid <- which(nvar <= q)
          if (length(valid) == 0) {
            cli::cli_warn("Even the most regularized version had > q vars.")
            # even the most regularised solution has > q vars; take it anyway
            idx <- 1
          } else {
            idx <- max(valid) # least regularised with <= q vars
          }
          # cli_abort(
          #   "need to figure out a way to get the betas in an organized way from the row with optimal lambda given q restriction"
          # )
          list(beta = beta[, idx], selected = as.numeric(beta[, idx] != 0))
        },
        error = function(e) NULL
      )

      selected <- rtn$selected

      if (is.null(selected)) {
        next
      }

      if (i %in% 1 & half == half1) {
        beta_at_q <- matrix(rtn$beta, nrow = 1)
      } else {
        beta_at_q <- rbind(beta_at_q, matrix(rtn$beta, nrow = 1))
      }
      sel_count <- sel_count + selected
      total_fits <- total_fits + 1
    }

    if (i %% 10 == 0 & verbose) {
      cat(sprintf("Completed %d / %d subsample pairs\n", i, nsub))
    }
  }

  colnames(beta_at_q) <- colnames(x)

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
