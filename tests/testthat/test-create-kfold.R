
# Setup -----
expect_positive_integer_scalar <- function(func, args, arg_name, msg) {
  # this is instead of quasi_label(), which requires rlang::enquo()
  act <- list(val = func, lab = encodeString(func, quote = "\""))

  args[[arg_name]] <- "4"    # character
  expect_error(do.call(func, args), msg)

  args[[arg_name]] <- matrix(1:4L, 2L, 2L)   # 2x2 matrix
  expect_error(do.call(func, args), msg)

  args[[arg_name]] <- c(2L, 4L)  # integer too long
  expect_error(do.call(func, args), msg)

  args[[arg_name]] <- 0   # < 0
  expect_error(do.call(func, args), msg)

  args[[arg_name]] <- 0.5   # not integer
  expect_error(do.call(func, args), msg)

  succeed()
  invisible(act$val)
}

expect_idx <- function(func, args) {
  act <- list(val = func, lab = encodeString(func, quote = "\""))
  msg <- "`idx` must be an integer vector."
  args$idx <- matrix(1L, 1L, 1L)
  expect_error(do.call(func, args), msg)

  args$idx <- integer()
  expect_error(do.call(func, args), msg)

  args$idx <- c(-1L, 2L, 3L, 4L)
  expect_error(do.call(func, args), msg)

  args$idx <- c(0L, 2L, 3L, 4L)
  expect_error(do.call(func, args), msg)

  succeed()
  invisible(act$val)
}

# Testing ----
# .make_strata() ----
# This function is the meat of the stratification
#   step and thus should always be tested.
test_that("`.make_strata()` returns expected errors", {

  expect_error(.make_strata(x = matrix(1:2, 1L)), "`x` must be a vector.")
  expect_error(.make_strata(x = numeric()), "`x` must be a vector.")
  expect_error(.make_strata(1:10, breaks = list(1, 2)),
               "`breaks` must be an integer or a numeric vector.")
  expect_error(.make_strata(1:10, breaks = matrix(1:2, 1L)),
               "`breaks` must be an integer or a numeric vector.")
  expect_error(.make_strata(1:10, breaks = c(NA, 2)),
               "`breaks` must be an integer or a numeric vector.")
  expect_error(.make_strata(1:10, breaks = c(NaN, 2)),
               "`breaks` must be an integer or a numeric vector.")
  expect_error(.make_strata(1:10, breaks = NaN),
               "`breaks` must be an integer or a numeric vector.")

  expect_positive_integer_scalar(".make_strata",
                                 list(x = 1:10, depth = 20L),
                                 "depth",
                                 "`depth` must be a positive integer.")

  expect_positive_integer_scalar(".make_strata",
                                 list(x = 1:10, n_unique = 5L),
                                 "n_unique",
                                 "`n_unique` must be a positive integer.")

  # discrete vector with > n_unique values, but breaks not provided
  expect_error(.make_strata(rep(1:10, 10L), breaks = NA),
               "`x` has 10 unique values. `breaks` cannot be NA.")

  # numeric vector with a breaks vector defined that does not cover values
  expect_error(.make_strata(withr::with_seed(42L, stats::runif(100)),
                            breaks = c(0.25, 0.5, 0.75)),
               "Provided `breaks` does not span data.")
})

test_that("`.make_strata()` returns expected results and warnings", {
  # A discrete variable with 5 or fewer values should return as an unmodified
  # character factor vector
  x <- withr::with_seed(1234L, sample(1:5, 100, TRUE))
  expect_equal(.make_strata(x), factor(as.character(x)))

  # A character variable with any number of values should return as an unmodified
  # character factor vector
  x <- withr::with_seed(1234L, sample(letters[1:8], 100, TRUE))
  expect_equal(.make_strata(x), factor(x))

  # A factor variable with any number of values should return as an unmodified
  # character factor vector
  x <- factor(withr::with_seed(1234L, sample(1:10, 100, TRUE)))
  expect_equal(.make_strata(x), factor(as.character(x)))

  # numeric vector with depth large enough to change break to >= 2
  x <- withr::with_seed(42L, stats::runif(100))
  expect_snapshot( out <- .make_strata(x, breaks = 10, depth = 50L) )

  breaks <- quantile(x, probs = c(0.0, 0.5, 1.0))
  expected <- cut(x, breaks = breaks, include.lowest = TRUE)
  expect_equal(out, expected)
  expect_equal(.make_strata(x, breaks = breaks, depth = 50L), expected)

  # numeric vector with depth large enough to change break to < 2
  expect_snapshot( out <- .make_strata(x, breaks = 10, depth = 70L) )

  expect_equal(out, factor(rep("strata1", 100)))

  # numeric vector with depth small enough to leave breaks unchanged
  breaks <- quantile(x, probs = c(0.0, 0.25, 0.5, 0.75, 1.0))
  expected <- cut(x, breaks = breaks, include.lowest = TRUE)
  expect_equal(.make_strata(x, breaks = 4, depth = 10L), expected)
  expect_equal(.make_strata(x, breaks = breaks, depth = 10L), expected)

  # imputed values continuous
  x <- c(x, NA_real_)
  expected2 <- factor(append(as.character(expected),
                             withr::with_seed(2345L, sample(levels(expected), 1))),
                      levels = levels(expected))
  expect_message(
    out <- withr::with_seed(2345L, .make_strata(x, breaks = 4, depth = 10L)),
    "Imputed stratification structure for 1 missing value."
  )
  expect_equal(out, expected2)

  x <- c(x, NA_real_)
  expected2 <- factor(append(as.character(expected),
                             withr::with_seed(2345L, sample(levels(expected), 2))),
                      levels = levels(expected))
  expect_message(
    out <- withr::with_seed(2345L, .make_strata(x, breaks = breaks, depth = 10L)),
    "Imputed stratification structure for 2 missing values."
  )
  expect_equal(out, expected2)

  # imputed values discrete
  x <- c(0L, 1L, 0L, 1L, 0L, NA_integer_)
  expected <- factor(as.character(x))
  expected[6L] <- withr::with_seed(2345L, sample(levels(expected), 1))
  expect_message(
    out <- withr::with_seed(2345L, .make_strata(x, breaks = 4, depth = 10L)),
    "Imputed stratification structure for 1 missing value."
  )
  expect_equal(out, expected)

  # numeric vector with > 5 values but # of breaks results in repeated quantiles
  x        <- c(0.1, 0.2, 0.3, 0.4, rep(0:1, 50))
  breaks   <- c(0.0, 0.25, 1.0)
  expected <- cut(x, breaks = breaks, include.lowest = TRUE)
  expect_equal(.make_strata(x, breaks = 4, depth = 1L), expected)
  expect_equal(.make_strata(x, breaks = c(0.0, 0.0, 0.25, 1.0, 1.0), depth = 1L),
               expected)
})

# .getStrataIndices_one() ----
# This function calls `.make_strata()` and manipulates the output. Provided
# that `.make_strata()` passes it checks, this function should not be affected.
# These tests can be moved to a less frequent schedule, though I would keep the
# error tests in place for all testing levels
test_that("`.getStrataIndices_one()` returns expected errors", {

  args <- list(x      = withr::with_seed(42L, sample(1L:2L, 30L, TRUE)),
               breaks = 4L,
               k      = 10L,
               idx    = 1:100L,
               depth  = 20L)

  expect_positive_integer_scalar(".getStrataIndices_one", args, "k",
                                 "`k` must be a positive integer.")

  expect_idx(".getStrataIndices_one", args)
})

test_that("`.getStrataIndices_one()` returns expected results", {
  # local implementation of function
  .local_imp <- function(strata, breaks, v, idx, depth) {
    stratas <- data.frame(idx    = idx,
                          strata = .make_strata(strata, breaks = breaks,
                                                depth = depth))

    # this produces a list of length n_breaks
    stratas <- unname(split(stratas, stratas$strata))
    result <- vector("list", v)
    for ( i in stratas ) {
      folds <- sample(rep(1:v, length.out = nrow(i)))
      for ( j in seq_len(v) ) {
        result[[j]] <- c(result[[j]], i$idx[which(folds == j)])
      }
    }
    result
  }

  # ensure that all allows types of discrete inputs are recognized
  allowed_types <- c("as.numeric", "as.character", "as.factor")

  x <- withr::with_seed(42L, stats::rbinom(1000, 1, 0.3))
  for ( i in seq_along(allowed_types) ) {
    xt <- do.call(allowed_types[i], list(x))
    expect_equal(
      withr::with_seed(1234L,
        .getStrataIndices_one(xt, breaks = 2, k = 4L, idx = 1:1000, depth = 5L)
      ),
      withr::with_seed(1234L, .local_imp(xt, 2, 4, 1:1000, 5L))
    )
  }

  # continuous variable with number of bins
  x <- withr::with_seed(42L, stats::runif(1000))
  expect_equal(
    withr::with_seed(1234L,
      .getStrataIndices_one(x, breaks = 4L, k = 5L, idx = 1:1000, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, 4L, 5, 1:1000, 5L))
  )

  # continuous variable with vector of breaks
  x <- withr::with_seed(42L, stats::runif(1000))
  expect_equal(
    withr::with_seed(1234L,
      .getStrataIndices_one(x, breaks = c(0.0, 0.25, 0.75, 1.0),
                            k = 5L, idx = 1:1000, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, c(0.0, 0.25, 0.75, 1.0), 5, 1:1000, 5L))
  )
})


# .getStrataIndices_two() ----
# This function calls `.make_strata()` and `.getStrataIndices_one()` (which
# in turn calls `.make_strata()`) and manipulates the output. Provided that
# `.make_strata()` passes it checks, this function should not be affected.
# These tests can be moved to a less frequent schedule, though I would keep the
# error tests in place for all testing levels
test_that("`.getStrataIndices_two()` returns expected errors", {
  strata <- simdata[, c("time", "status")]
  breaks <- list(time = 4L, status = NA)

  expect_error(.getStrataIndices_two(matrix(1:30, ncol = 1L),
                                     breaks = breaks, k = 4L, idx = 1:30L,
                                     depth = 2L),
               "`x` must be a `data.frame` with 2 columns.")
  expect_error(.getStrataIndices_two(data.frame("x" = 1, "Y" = 2, "Z" = 3),
                                     breaks = breaks, k = 4L, idx = 1:30L,
                                     depth = 2L),
               "`x` must be a `data.frame` with 2 columns.")
  expect_error(.getStrataIndices_two(1:3,
                                     breaks = breaks, k = 4L, idx = 1:30L,
                                     depth = 2L),
               "`x` must be a `data.frame` with 2 columns.")

  expect_error(.getStrataIndices_two(strata, breaks = breaks[[1L]], k = 4L,
                                     idx = 1:30L, depth = 2L),
               "`breaks` must be a list of length 2.")
  expect_error(.getStrataIndices_two(strata, breaks = NaN,
                                     k = 4L, idx = 1:30L, depth = 2L),
               "`breaks` must be a list of length 2.")
  expect_error(.getStrataIndices_two(strata, breaks = list(1, 2, 3),
                                     k = 4L, idx = 1:30L, depth = 2L),
               "`breaks` must be a list of length 2.")
  expect_error(.getStrataIndices_two(strata, breaks = list(),
                                     k = 4L, idx = 1:30L, depth = 2L),
               "`breaks` must be a list of length 2.")

  args <- list(x      = strata,
               breaks = breaks,
               k      = 10L,
               idx    = 1:100L,
               depth  = 20L)

  expect_positive_integer_scalar(".getStrataIndices_two", args, "k",
                                 "`k` must be a positive integer.")

  expect_idx(".getStrataIndices_one", args)
})

test_that("`.getStrataIndices_two()` returns expected results", {
  # local implementation
  .local_imp <- function(strata, breaks, k, idx, depth) {
    stratas_1 <- data.frame(idx    = idx,
                            strata = .make_strata(strata[, 1L],
                                                  breaks = breaks[[1L]],
                                                  depth  = depth))
    stratas_1 <- unname(split(stratas_1, stratas_1$strata))

    results <- vector("list", k)
    for ( i in seq_along(stratas_1) ) {
      tmp <- .getStrataIndices_one(strata[stratas_1[[i]]$idx, 2L],
                                   breaks = breaks[[2L]],
                                   k = k, idx = stratas_1[[i]]$idx,
                                   depth = depth)
      for ( j in seq_len(k) ) {
        results[[j]] <- c(results[[j]], tmp[[j]])
      }

    }
    results
  }

  # data.frame with two discrete variables
  x <- withr::with_seed(42L, data.frame(s1 = stats::rbinom(1000, 1, 0.5),
                                        s2 = stats::rbinom(1000, 1, 0.3)))
  # breaks should be ignored -- provide an inappropriate value to ensure
  breaks <- list(s1 = 4L, s2 = c(0.25, 0.5, 1.0))
  expect_equal(
    withr::with_seed(1234L,
      .getStrataIndices_two(x, breaks = breaks, k = 4L, idx = 1:1000, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, list(NA, NA), 4L, 1:1000, 5L))
  )

  # data.frame with one continuous and one discrete
  x <- withr::with_seed(42L, data.frame(s1 = stats::runif(1000),
                                        s2 = stats::rbinom(1000, 1, 0.3)))
  # breaks[[1]] is number of bins
  # breaks[[2]] should be ignored -- provide an inappropriate value to ensure
  breaks <- list(s1 = 4L, s2 = c(0.25, 0.5, 1.0))
  expect_equal(
    withr::with_seed(1234L,
      .getStrataIndices_two(x, breaks = breaks, k = 4L, idx = 1:1000, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, list(4, NA), 4L, 1:1000, 5L))
  )

  # breaks[[1]] is a vector of bin boundaries
  # breaks[[2]] should be ignored -- provide an inappropriate value to ensure
  breaks <- list(s1 = c(0.0, 0.5, 1.0), s2 = c(0.25, 0.5, 1.0))
  expect_equal(
    withr::with_seed(1234L,
      .getStrataIndices_two(x, breaks = breaks, k = 5L, idx = 1:1000, depth = 5L)
    ),
    withr::with_seed(1234L,
      .local_imp(x, breaks = list(breaks[[1L]], NA), k = 5L, idx = 1:1000, depth = 5L)
    )
  )

  # two continuous variables with all combination of breaks
  x <- withr::with_seed(42L, data.frame(s1 = stats::runif(1000),
                                        s2 = stats::runif(1000)))

  breaks <- list(s1 = 4L, s2 = 5L)
  expect_equal(
    withr::with_seed(1234L,
      .getStrataIndices_two(x, breaks = breaks, k = 4L, idx = 1:1000, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, list(4, 5), 4L, 1:1000, 5L))
  )

  breaks <- list(s1 = c(0.0, 0.6, 1.0), s2 = 5L)
  expect_equal(
    withr::with_seed(1234L,
      .getStrataIndices_two(x, breaks = breaks, k = 4L, idx = 1:1000, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, list(breaks[[1L]], 5), 4L, 1:1000, 5L))
  )

  breaks <- list(s1 = 4L, s2 = c(0.0, 0.6, 0.8, 1.0))
  expect_equal(
    withr::with_seed(1234L,
      .getStrataIndices_two(x, breaks = breaks, k = 4L, idx = 1:1000, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, list(4, breaks[[2]]), 4L, 1:1000, 5L))
  )

  breaks <- list(s1 = c(0.0, 0.5, 1.0), s2 = c(0.0, 0.6, 0.8, 1.0))
  expect_equal(
    withr::with_seed(1234L,
      .getStrataIndices_two(x, breaks = breaks, k = 4L, idx = 1:1000, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, breaks, 4L, 1:1000, 5L))
  )
})

test_that("`.getStrataIndices()` returns expected results for strata = NULL", {
  folds <- withr::with_seed(42L, sample(rep(1:3L, length.out = 30L)))
  expected <- list(
    (1:30L)[folds == 1L],
    (1:30L)[folds == 2L],
    (1:30L)[folds == 3L]
  )

  expect_equal(
    withr::with_seed(42L, .getStrataIndices(NULL, 3L, 1:30L, NULL)),
    expected
  )

  idx <- withr::with_seed(1234L, sample(1:30L, 30L, TRUE))
  folds <- withr::with_seed(42L, sample(rep(1:4L, length.out = 30L)))
  expected <- list(
    idx[folds == 1L],
    idx[folds == 2L],
    idx[folds == 3L],
    idx[folds == 4L]
  )

  expect_equal(
    withr::with_seed(42L, .getStrataIndices(NULL, 4L, idx, NULL)),
    expected
  )
})

test_that("`.getStrataIndices()` returns expected results for data.frame strata", {
  # data.frame of two continuous
  x <- withr::with_seed(42L, data.frame(s1 = stats::runif(1000),
                                        s2 = stats::runif(1000)))
  breaks <- list(s1 = 4L, s2 = 4L)
  expect_equal(
    withr::with_seed(1234L,
      .getStrataIndices(x, breaks = breaks, k = 4L, idx = 1:1000, depth = 5L)
    ),
    withr::with_seed(1234L,
      .getStrataIndices_two(x, breaks = breaks, k = 4L, idx = 1:1000, depth = 5L)
    )
  )

  # data.frame of one continuous
  expect_equal(
    withr::with_seed(1234L,
      .getStrataIndices(x[, 1L, drop = FALSE], breaks = breaks[1:1L], k = 4L,
                        idx = 1:1000, depth = 5L)
    ),
    withr::with_seed(1234L,
      .getStrataIndices_one(x[, 1L], breaks = 4, k = 4L, idx = 1:1000, depth = 5L)
    )
  )

  # data.frame with factor variable
  x <- withr::with_seed(42L, data.frame(s1 = factor(sample(c("a", "d"), 1000, TRUE))))

  # passing inappropriate breaks value, should be ignored
  expect_equal(
    withr::with_seed(1234L,
      .getStrataIndices(x, breaks = list(s1 = c(0.0, 0.25, 0.75, 1.0)),
                        k = 5L, idx = 1:1000, depth = 5L)
    ),
    withr::with_seed(1234L,
      .getStrataIndices_one(x[, 1L], breaks = c(0.0, 0.25, 0.75, 1.0),
                            k = 5L, idx = 1:1000, depth = 5L)
    )
  )
})

# .vfold_splits ----
# This function calls `.getStrataIndices()` and manipulates the output. Provided
# that `.make_strata()` passes it checks, this function should not be affected.
# These tests can be moved to a less frequent schedule, though I would keep the
# error tests in place for all testing levels
test_that("`.vfold_splits()` returns expected errors", {
  expect_error(
    .vfold_splits(k = 10L, breaks = NULL, depth = 20L),
    "`data` must be a `data.frame`."
  )
  expect_error(
    .vfold_splits(simdata$time, k = 10L, breaks = NULL, depth = 20L),
    "`data` must be a `data.frame`."
  )

  expect_positive_integer_scalar(".vfold_splits",
                                 list(data   = simdata,
                                      breaks = NULL,
                                      k      = 10L,
                                      depth  = 20L),
                                 "k",
                                 "`k` must be a positive integer.")

  expect_error(
    .vfold_splits(simdata, k = 10L, breaks = 4L, depth = 20L),
    "`breaks` must be NULL or a list of length 1 or 2."
  )
  expect_error(
    .vfold_splits(simdata, k = 10L, breaks = NA, depth = 20L),
    "`breaks` must be NULL or a list of length 1 or 2."
  )
  expect_error(
    .vfold_splits(simdata, k = 10L, breaks = list("a", "b", "c"), depth = 20L),
    "`breaks` must be NULL or a list of length 1 or 2."
  )
  expect_error(
    .vfold_splits(simdata, k = 10L, breaks = list(), depth = 20L),
    "`breaks` must be NULL or a list of length 1 or 2."
  )
})

test_that("`.vfold_splits()` returns expected results", {
  # local implementation
  .local_imp <- function(strata, breaks, k, n, depth) {
    indices <- .getStrataIndices(strata, breaks = breaks, k = k, idx = seq_len(n),
                                 depth = depth)
    indices_2 <- vector("list", length(indices))
    for ( i in seq_along(indices) ) {
      indices_2[[i]] <- list(analysis   = setdiff(1L:n, indices[[i]]),
                             assessment = sort(unique(indices[[i]])))
    }
    tibble::tibble(split = indices_2, Fold = seq_len(k))
  }

  # No stratification
  expect_equal(
    withr::with_seed(1234L, .vfold_splits(simdata)),
    withr::with_seed(1234L,
      .local_imp(NULL, breaks = NULL, k = 10L, n = nrow(simdata), depth = 20L)
    )
  )
  expect_equal(
    withr::with_seed(1234L, .vfold_splits(simdata, k = 4L)),
    withr::with_seed(1234L,
      .local_imp(NULL, breaks = NULL, k = 4L, n = nrow(simdata), depth = 20L)
    )
  )

  # stratify on 1 variable with # of bins
  expect_equal(
    withr::with_seed(1234L,
      .vfold_splits(simdata, k = 4L, breaks = list(time = 4))
    ),
    withr::with_seed(1234L,
      .local_imp(simdata[, "time", drop = FALSE], breaks = list(4L), k = 4L,
                 n = nrow(simdata), depth = 20L)
    )
  )

  # stratify on 1 variable with vector of bins
  breaks_vec <- seq(min(simdata$time), max(simdata$time),
                    length.out = 5)
  expect_equal(
    withr::with_seed(1234L,
      .vfold_splits(simdata, k = 4L, breaks = list(time = breaks_vec))
    ),
    withr::with_seed(1234L,
      .local_imp(simdata[, "time", drop = FALSE], breaks = list(breaks_vec),
                 k = 4L, n = nrow(simdata), depth = 20L)
    )
  )

  # stratify on 2 variable with all combinations of breaks
  expect_equal(
    withr::with_seed(1234L,
      .vfold_splits(simdata, k = 4L, breaks = list(time = 5L, status = 100L))
    ),
    withr::with_seed(1234L,
      .local_imp(simdata[, c("time", "status")], breaks = list(5L, NA),
                 k = 4L, n = nrow(simdata), depth = 20L)
    )
  )
  expect_equal(
    withr::with_seed(1234L,
      .vfold_splits(simdata, k = 4L, breaks = list(time = breaks_vec,
                                                        status = NA))
    ),
    withr::with_seed(1234L,
      .local_imp(simdata[, c("time", "status")], breaks = list(breaks_vec, NA),
                 k = 4L, n = nrow(simdata), depth = 20L)
    )
  )
})

# create_kfold ----
# These are the tests for the primary function. Always run all tests.
test_that("`create_kfold()` returns expected errors", {
  expect_error(
    create_kfold(k = 10L, strata = NULL, breaks = 4L),
    "`data` must be a `data.frame`."
  )
  expect_error(
    create_kfold(simdata$time),
    "`data` must be a `data.frame`."
  )

  expect_positive_integer_scalar("create_kfold",
                                 list(data = simdata, k = 10L),
                                 "k",
                                 "`k` must be a positive integer.")

  expect_positive_integer_scalar("create_kfold",
                                 list(data = simdata, repeats = 10L),
                                 "repeats",
                                 "`repeats` must be a positive integer.")

  expect_error(
    create_kfold(simdata, breaks = list()),
    "`breaks` must be NULL or a list of length 1 or 2."
  )
  expect_error(
    create_kfold(simdata, breaks = list("a", "b", "c")),
    "`breaks` must be NULL or a list of length 1 or 2."
  )
  expect_error(
    create_kfold(simdata, breaks = NA),
    "`breaks` must be NULL or a list of length 1 or 2."
  )
})

test_that("`create_kfold()` returns expected results for repeats = 1", {
  # local implementation
  .local_imp <- function(data, k, breaks, depth) {
    split_objs <- .vfold_splits(data = data, k = k, breaks = breaks, depth = depth)
    split_objs$Repeat <- NA_integer_
    return_obj <- list(data = data, splits = split_objs)
    structure(add_class(return_obj, "x_split"), breaks = breaks)
  }

  # test that depth is properly understood
  # easiest check a case that will generate a warning
  expect_snapshot(
    out <- withr::with_seed(234L,
      create_kfold(simdata, breaks = list(time = 4L), depth  = 70L))
  )

  expect_equal(
    out,
    withr::with_seed(234L,
      suppressWarnings(.local_imp(data   = simdata, k = 10L,
                                  breaks = list(time = 4L), depth = 70L))),
    ignore_attr = TRUE   # call attr differs
  )

  # each input is properly passed along
  expect_equal(
    withr::with_seed(1234L, create_kfold(simdata, k = 5L)),
    withr::with_seed(1234L,
      .local_imp(data = simdata, k = 5L, breaks = NULL, depth = 20L)
    ),
    ignore_attr = TRUE   # call attr differs
  )

  expect_equal(
    withr::with_seed(1234L, create_kfold(simdata, breaks = list(time = 4L))),
    withr::with_seed(1234L,
      .local_imp(data = simdata, k = 10L, breaks = list(time = 4L), depth = 10L)
    ),
    ignore_attr = TRUE   # call attr differs
  )

  expect_equal(
    withr::with_seed(1234L, create_kfold(simdata, breaks = list(time = 5L))),
    withr::with_seed(1234L,
      .local_imp(data = simdata, k = 10L, breaks = list(time = 5L), depth = 20L)
    ),
    ignore_attr = TRUE   # call attr differs
  )
})

test_that("`create_kfold()` returns expected results for repeats != 1", {
  # local implementation
  .local_imp <- function(data, k, repeats, breaks, depth) {
    split_objs <- lapply(seq_len(repeats),
                         function(.i) {
                           tmp <- .vfold_splits(data = data, k = k,
                                                breaks = breaks, depth = depth)
                           tmp$Repeat <- .i
                           tmp
                         })
    names(split_objs) <- seq_len(repeats)
    split_objs <- split_objs |> dplyr::bind_rows() |> tibble::tibble()
    return_obj <- list(data = data, splits = split_objs)
    structure(add_class(return_obj, "x_split"), breaks = breaks)
  }

  # each input is properly passed along
  expect_equal(
    withr::with_seed(1234L, create_kfold(simdata, k = 5L, repeats = 2L)),
    withr::with_seed(1234L,
      .local_imp(simdata, k = 5L, repeats = 2, breaks = NULL, depth = 20L)
    ),
    ignore_attr = TRUE   # call attribute will differ
  )
})

# assessment ----
# This one can go either way. Provided that the create_kfolds() tests are
# successful, this should not change. Probably safe to only run these
# in the nightly.
test_that("`assessment()` returns expected errors", {
  split_obj <- create_kfold(simdata, k = 4L)
  expect_error(
    assessment(simdata),
    "`object` must be a `x_split` object."
  )
  expect_positive_integer_scalar("assessment",
                                 list(object = split_obj, i = 10L),
                                 "i",
                                 "`i` must be a positive integer.")
  expect_error(
    assessment(split_obj, i = 5L),
    "`i` cannot exceed the number of splits."
  )
})

test_that("`assessment()` returns expected results", {
  split_obj <- create_kfold(simdata, k = 4L)
  expected  <- vector("list", 4L)
  for ( i in seq_len(4L) ) {
    expected[[i]] <- simdata[split_obj$splits$split[[i]]$assessment, ]
  }
  expect_length(out <- assessment(split_obj), 4L)
  expect_equal(out, expected)
  expect_equal(assessment(split_obj, i = 1L), list(expected[[1L]]))
  expect_equal(assessment(split_obj, i = 2L), list(expected[[2L]]))
  expect_equal(assessment(split_obj, i = 3L), list(expected[[3L]]))
  expect_equal(assessment(split_obj, i = 4L), list(expected[[4L]]))
})

# analysis ----
# This one can go either way. Provided that the create_kfolds tests are
# successful, this should not change. Probably safe to only run these
# in the nightly.
test_that("`analysis()` returns expected errors", {
  split_obj <- create_kfold(simdata, k = 4L)
  expect_error(
    analysis(simdata),
    "`object` must be a `x_split` object."
  )
  expect_positive_integer_scalar("analysis",
                                 list(object = split_obj, i = 10L),
                                 "i",
                                 "`i` must be a positive integer.")
  expect_error(
    analysis(split_obj, i = 5L),
    "`i` cannot exceed the number of splits."
  )
})

test_that("`analysis()` returns expected results", {
  split_obj <- create_kfold(simdata, k = 4L)
  expected <- vector("list", 4L)
  for ( i in seq_len(4L) ) {
    expected[[i]] <- simdata[split_obj$splits$split[[i]]$analysis, ]
  }

  expect_length(out <- analysis(split_obj), 4L)
  expect_equal(out, expected)
  expect_equal(analysis(split_obj, i = 1L), list(expected[[1L]]))
  expect_equal(analysis(split_obj, i = 2L), list(expected[[2L]]))
  expect_equal(analysis(split_obj, i = 3L), list(expected[[3L]]))
  expect_equal(analysis(split_obj, i = 4L), list(expected[[4L]]))
})

# print.x_split() ----
test_that("`S3 print()` returns expected class `x_split`", {

  # no stratification; no repeats
  expect_snapshot(
    create_kfold(simdata, k = 4L, repeats = 1L)
  )

  # no stratification; w/ repeats
  expect_snapshot(
    create_kfold(simdata, k = 4L, repeats = 3L)
  )

  # no stratification; no repeats; based on a data.frame
  expect_snapshot({
    df <- as.data.frame(simdata[, c("time", "status")])
    create_kfold(df, k = 5L)
  })

  # stratification on 1 discrete variable
  expect_snapshot(
    create_kfold(simdata, k = 5L, breaks = list(status = NA))
  )

  # stratification on 2 variables; 1 continuous + 1 discrete
  expect_snapshot(
    create_kfold(simdata, k = 5L, repeats = 3L,
                 breaks = list(time = 4L, status = NA))
  )
})

# is.x_split ----
# Safe to only run these in the nightly.
test_that("`is.x_split()` returns expected results", {
  obj <- create_kfold(simdata, k = 5L, repeats = 3L,
                      breaks = list(time = 4L, status = NA))
  expect_true(is.x_split(obj))
  expect_false(is.x_split(unclass(obj)))
})
