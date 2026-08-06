
# Tests for `impute_missing()`
#   and internal `fit_*_mle()` helpers

# impute_missing() -----
test_that("`impute_missing()` fills NAs for the normal distribution", {
  withr::with_seed(101, {
    x <- rnorm(50, mean = 10, sd = 2)
    x[c(3L, 10L, 20L)] <- NA_real_
  })
  y <- impute_missing(x, distr = "norm")
  expect_type(y, "double")
  expect_length(y, 50L)
  expect_equal(sum(is.na(y)), 0L)
  # non-NA positions unchanged
  keep <- !is.na(x)
  expect_equal(y[keep], x[keep])
})

test_that("`impute_missing()` fills NAs for the gamma distribution", {
  withr::with_seed(101, {
    x <- rgamma(50L, shape = 2, rate = 1)
    x[c(5L, 15L, 25L)] <- NA_real_
  })
  y <- impute_missing(x, distr = "gamma")
  expect_length(y, 50L)
  expect_equal(sum(is.na(y)), 0L)
  # gamma draws must be strictly positive
  expect_true(all(y > 0))
})

test_that("`impute_missing()` fills NAs for the beta distribution", {
  withr::with_seed(101, {
    x <- rbeta(50L, shape1 = 2, shape2 = 5)
    x[c(5L, 15L, 25L)] <- NA_real_
  })
  y <- impute_missing(x, distr = "beta")
  expect_length(y, 50L)
  expect_equal(sum(is.na(y)), 0L)
  # beta draws in (0, 1)
  expect_true(all(y > 0 & y < 1))
})

test_that("`impute_missing()` is reproducible via the `seed` argument", {
  withr::with_seed(1, {
    x <- rnorm(50L)
    x[c(3L, 10L)] <- NA_real_
  })
  a <- impute_missing(x, distr = "norm", seed = 42L)
  b <- impute_missing(x, distr = "norm", seed = 42L)
  z <- impute_missing(x, distr = "norm", seed = 99L)
  expect_equal(a, b)
  expect_false(identical(a, z))
})

test_that("`impute_missing()` no-ops when there are no NAs", {
  withr::with_seed(1, x <- rnorm(50L))
  y <- impute_missing(x, distr = "norm")
  expect_equal(y, x)
})

test_that("`impute_missing()` errors on an unknown `distr` value", {
  expect_error(impute_missing(rnorm(50L), distr = "poisson"),
               "`distr` should be one of")
})

test_that("`impute_missing()` default method errors on unsupported class", {
  expect_error(impute_missing(list(1, 2, 3)),
               "Couldn't find a S3 method")
  expect_error(impute_missing("a string"),
               "Couldn't find a S3 method")
})


# fit_norm_mle() -----
test_that("`fit_norm_mle()` recovers known parameters", {
  withr::with_seed(101, x <- rnorm(500L, mean = 10, sd = 2))
  fit <- fit_norm_mle(x)
  expect_s3_class(fit, "tbl_df")
  expect_named(fit, c("method", "estimator", "estimate",
                      "mean", "sd", "converged"))
  expect_true(fit$converged)
  expect_equal(fit$mean, 9.87443, tolerance = 1e-05)
  expect_equal(fit$sd,   1.93073, tolerance = 1e-05)
})

test_that("`fit_norm_mle()` ignores NAs via `.prep_fit()`", {
  withr::with_seed(101, x <- rnorm(50L, mean = 5, sd = 1))
  x[c(1L, 2L)] <- NA_real_
  fit <- fit_norm_mle(x)
  expect_true(fit$converged)
  expect_false(is.na(fit$mean))
})


# fit_gamma_mle() -----
test_that("`fit_gamma_mle()` recovers known parameters", {
  withr::with_seed(101, x <- rgamma(500L, shape = 2, rate = 1))
  fit <- fit_gamma_mle(x)
  expect_s3_class(fit, "tbl_df")
  expect_named(fit, c("method", "estimator", "estimate",
                      "shape", "rate", "scale", "converged"))
  expect_true(fit$converged)
  expect_equal(fit$shape, 1.777825, tolerance = 1e-05)
  expect_equal(fit$rate, 0.8762169, tolerance = 1e-05)
  expect_equal(fit$scale, 1 / fit$rate) # scale ~> 1 / rate
})

test_that("`fit_gamma_mle()` replaces exact zeros before fitting", {
  # Zeros make log-likelihood -Inf; the internal
  # `x[x == 0] <- 1e-04` guard should prevent that.
  withr::with_seed(1, x <- rgamma(50L, shape = 2, rate = 1))
  x[c(3L, 10L)] <- 0
  fit <- fit_gamma_mle(x)
  expect_true(fit$converged)
  expect_true(is.finite(fit$estimate))
})


# fit_beta_mle() -----
test_that("`fit_beta_mle()` recovers known parameters", {
  withr::with_seed(101, x <- rbeta(500L, shape1 = 2, shape2 = 5))
  fit <- fit_beta_mle(x)
  expect_s3_class(fit, "tbl_df")
  expect_named(fit, c("method", "estimator", "estimate",
                      "shape1", "shape2", "converged"))
  expect_true(fit$converged)
  expect_equal(fit$shape1, 2.091034, tolerance = 1e-05)
  expect_equal(fit$shape2, 5.346894, tolerance = 1e-05)
})

test_that("`fit_beta_mle()` trims exact 0 and 1 endpoints", {
  # dbeta is -Inf at exactly 0 or 1; internal trims should
  # keep the log-likelihood finite.
  withr::with_seed(1, x <- rbeta(50L, shape1 = 2, shape2 = 5))
  x[3L] <- 0
  x[10L] <- 1
  fit <- fit_beta_mle(x)
  expect_true(fit$converged)
  expect_true(is.finite(fit$estimate))
})


# .prep_fit() -----
test_that("`.prep_fit()` requires at least 10 observations", {
  expect_error( .prep_fit(1:9), "length\\(x\\) >= 10 is not TRUE" )
  expect_silent(.prep_fit(1:10))
})

test_that("`.prep_fit()` drops NAs from the input", {
  x <- c(1:10, NA_real_, NA_real_)
  expect_length(x, 12L)
  out <- .prep_fit(x)
  expect_equal(out, 1:10)
  expect_length(out, 10L)
  expect_true(all(!is.na(out)))
})


# .get_dist_params() -----
test_that("`.get_dist_params()` pulls the correct fields per distr", {
  fit_n <- tibble::tibble(mean = 1, sd = 2, extra = 99)
  fit_g <- tibble::tibble(shape = 1, rate = 2, scale = 0.5, extra = 99)
  fit_b <- tibble::tibble(shape1 = 1, shape2 = 2, extra = 99)

  expect_equal(.get_dist_params(fit_n, "norm"),
               list(mean = 1, sd = 2))
  expect_equal(.get_dist_params(fit_g, "gamma"),
               list(shape = 1, rate = 2))
  expect_equal(.get_dist_params(fit_b, "beta"),
               list(shape1 = 1, shape2 = 2))
})
