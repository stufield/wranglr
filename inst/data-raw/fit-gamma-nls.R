
fit_gamma_nls <- function(x) {
  x <- prep_gamma(x)
  mu <- mean(x)
  s2 <- var(x)
  start <- c(shape = mu^2 / s2, rate = mu / s2)

  fit <- stats::nls(
    ecdf_vals ~ pgamma(x_sorted, shape, rate),
    start = as.list(start), algorithm = "port",
    lower = c(shape = 1e-06, rate = 1e-06),
    control = nls.control(maxiter   = 2000,
                          minFactor = 1 / 1024,
                          warnOnly  = TRUE)
  )
  tibble(
    method    = "NLS (ECDF)",
    estimator = "SSE",
    estimate  = sum(fit$m$resid()^2),
    shape     = coef(fit)[["shape"]],
    rate      = coef(fit)[["rate"]],
    scale     = 1 / rate,  # nolint: object_usage_linter.
    converged = fit$convergence == 0
  )
}
