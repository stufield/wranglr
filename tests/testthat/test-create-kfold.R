
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
# .create_strata() ----
# This function is the meat of the stratification
#   step and thus should always be tested.
test_that("`.create_strata()` returns expected errors", {

  expect_error(.create_strata(x = matrix(1:2, 1L)), "`x` must be a numeric vector.")
  expect_error(.create_strata(x = numeric()), "`x` must be a numeric vector.")
  x <- sample(c("a", "b"), 100, TRUE)
  expect_error(.create_strata(x), "`x` must be a numeric vector.")
  expect_error(.create_strata(factor(x)), "`x` must be a numeric vector.")

  x <- seq(1, 10, length.out = 100L)  # dummy numeric vector
  expect_error(.create_strata(x, breaks = list(1, 2)),
               "`breaks` must be an integer or a numeric vector.")
  expect_error(.create_strata(x, breaks = matrix(1:2, 1L)),
               "`breaks` must be an integer or a numeric vector.")
  expect_error(.create_strata(x, breaks = c(NA, 2)),
               "`breaks` must be an integer or a numeric vector.")
  expect_error(.create_strata(x, breaks = c(NaN, 2)),
               "`breaks` must be an integer or a numeric vector.")
  expect_error(.create_strata(x, breaks = NaN),
               "`breaks` must be an integer or a numeric vector.")

  expect_positive_integer_scalar(".create_strata",
                                 list(x = x, depth = 20L),
                                 "depth",
                                 "`depth` must be a positive integer.")

  expect_positive_integer_scalar(".create_strata",
                                 list(x = x, n_unique = 5L),
                                 "n_unique",
                                 "`n_unique` must be a positive integer.")

  # discrete vector with > n_unique values, but breaks not provided
  expect_error(
    .create_strata(x, breaks = NA),
    "`breaks` must be an integer or a numeric vector."
  )

  # numeric vector with a breaks vector defined that does not cover values
  expect_error(.create_strata(withr::with_seed(42L, stats::runif(100)),
                            breaks = c(0.25, 0.5, 0.75)),
               "Provided `breaks` does not span data.")
})

test_that("`.create_strata()` returns expected results and warnings", {
  # A discrete variable with 5 or fewer values should return as an unmodified
  # character factor vector
  x <- withr::with_seed(1234L, sample(1:5, 100, TRUE))
  expect_equal(.create_strata(x), factor(as.character(x)))


  # numeric vector with depth large enough to change break to >= 2
  x <- withr::with_seed(42L, stats::runif(100))
  expect_error(
    .create_strata(x, breaks = 10, depth = 50L),
    "Too little data to stratify."
  ) |> expect_warning("The number of observations in each quantile")

  breaks <- quantile(x, probs = c(0.0, 0.5, 1.0))
  expected <- cut(x, breaks = breaks, include.lowest = TRUE)
  expect_equal(.create_strata(x, breaks = breaks, depth = 50L), expected)

  # numeric vector with depth small enough to leave breaks unchanged
  breaks <- quantile(x, probs = c(0.0, 0.25, 0.5, 0.75, 1.0))
  expected <- cut(x, breaks = breaks, include.lowest = TRUE)
  expect_equal(.create_strata(x, breaks = 4, depth = 10L), expected)
  expect_equal(.create_strata(x, breaks = breaks, depth = 10L), expected)

  # imputed values continuous
  # imputed median is in the 2nd cut
  x <- c(x, NA_real_)
  expected2 <- c(expected, factor(levels(expected)[2L]))
  out <- withr::with_seed(2345L, .create_strata(x, breaks = 4, depth = 10L))
  expect_equal(out, expected2)

  x <- c(x, NA_real_)
  expected3 <- c(expected2, factor(levels(expected)[2L]))
  out <- withr::with_seed(2345L, .create_strata(x, breaks = breaks, depth = 10L))
  expect_equal(out, expected3)

  # imputed values discrete/character/factor
  x <- c(0L, 1L, 0L, 1L, 0L, NA_integer_)
  expected <- factor(as.character(x))
  expected[6L] <- withr::with_seed(2345L, sample(levels(expected), 1))
  expect_message(
    out <- withr::with_seed(2345L, .create_strata(x, breaks = 4, depth = 10L)),
    "Imputed stratification for 1 missing value."
  )
  expect_equal(out, expected)

  # numeric vector with > 5 values but # of breaks results in repeated quantiles
  x        <- c(0.1, 0.2, 0.3, 0.4, rep(0:1, 50))
  breaks   <- c(0.0, 0.25, 1.0)
  expected <- cut(x, breaks = breaks, include.lowest = TRUE)
  expect_equal(.create_strata(x, breaks = 4, depth = 1L), expected)
  expect_equal(.create_strata(x, breaks = c(0.0, 0.0, 0.25, 1.0, 1.0), depth = 1L),
               expected)
})


# .get_indices() numeric ----
# calls `.create_strata()` and manipulates the output.
test_that("`.get_indices() numeric S3 method returns expected errors", {
  expect_error(
    .get_indices(1:100, breaks = list(s1 = c(0.0, 0.25, 0.75, 1.0)),
                 k = 5L, idx = 1:100, depth = 5L),
    "`breaks` must be a vector of cutoffs, not a 'list'"
  )
  args <- list(x      = withr::with_seed(42L, sample(1L:2L, 30L, TRUE)),
               breaks = 4L,
               k      = 10L,
               idx    = 1:100L,
               depth  = 20L)

  expect_positive_integer_scalar(".get_indices.numeric", args, "k",
                                 "`k` must be a positive integer.")

  expect_idx(".get_indices.numeric", args)
})

test_that("`.get_indices() numeric S3 method returns expected results", {
  # local implementation of function
  .local_imp <- function(strata, breaks, v, idx, depth) {
    stratas <- data.frame(idx    = idx,
                          strata = .create_strata(strata, breaks = breaks,
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
  allowed_types <- c("as.numeric", "as.integer", "as.character", "as.factor")

  for ( i in seq_along(allowed_types) ) {
    xt <- do.call(allowed_types[i], list(seq(1, 10, length.out = 50)))
    expect_no_error(
     .get_indices(xt, breaks = 2, k = 4L, idx = 1:50, depth = 5L)
    )
  }

  # continuous variable with number of bins
  x <- withr::with_seed(42L, stats::runif(1000))
  expect_equal(
    withr::with_seed(1234L,
      .get_indices(x, breaks = 4L, k = 5L, idx = 1:1000, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, 4L, 5, 1:1000, 5L))
  )

  # continuous variable with vector of breaks
  x <- withr::with_seed(42L, stats::runif(1000))
  expect_equal(
    withr::with_seed(1234L,
      .get_indices(x, breaks = c(0.0, 0.25, 0.75, 1.0),
                   k = 5L, idx = 1:1000, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, c(0.0, 0.25, 0.75, 1.0), 5, 1:1000, 5L))
  )
})


test_that("`.get_indices()` default S3 method returns expected errors", {
  expect_error(
    .get_indices(matrix(1:30, ncol = 1L), breaks = breaks, k = 4L,
                 idx = 1:30L, depth = 2L),
    "`x` must be numeric, not a 'matrix'"
  )
  expect_error(
    .get_indices(1:3, breaks = breaks, k = 4L, idx = 1:30L, depth = 2L),
    "`idx` must be an integer vector with dimension matching `x`."
  )
})

test_that("`.get_indices()` factor and char S3 method returns expected values", {
  x <- sample(c("a", "d"), 1000, TRUE)
  out <- withr::with_seed(101L,
    .get_indices(x, k = 10L, idx = seq_along(x), depth = 20L))
  expect_type(out, "list")
  expect_equal(length(out), 10L)

  # same for factor class
  y <- factor(x)
  expect_equal(
    out,
    withr::with_seed(101L,
      .get_indices(y, k = 10L, idx = seq_along(y), depth = 20L))
  )
})


# .get_indices() data.frame ----
# This function calls `.create_strata()` and `.get_indices_one()` (which
# in turn calls `.create_strata()`) and manipulates the output.
test_that("`.get_indices()` data.frame S3 method returns expected errors", {
  strata <- simdata[, c("time", "status")]
  breaks <- list(time = 4L, status = NA)

  expect_error(
    .get_indices(data.frame("x" = 1, "Y" = 2, "Z" = 3),
                 breaks = breaks, k = 4L, idx = 1:30L, depth = 2L),
    "`x` must be a `data.frame` with 2 columns."
  )

  expect_error(
    .get_indices(strata, breaks = breaks[[1L]], k = 4L,
                 idx = 1:30L, depth = 2L),
    "`breaks` must be a list of length 2."
  )
  expect_error(
    .get_indices(strata, breaks = "foo", k = 4L, idx = 1:30L, depth = 2L),
    "`breaks` must be a list of length 2."
  )
  expect_error(
    .get_indices(strata, breaks = list(1, 2, 3), k = 4L,
                 idx = 1:30L, depth = 2L),
    "`breaks` must be a list of length 2."
  )
  expect_error(
    .get_indices(strata, breaks = list(), k = 4L, idx = 1:30L, depth = 2L),
    "`breaks` must be a list of length 2."
  )

  args <- list(x      = strata,
               breaks = breaks,
               k      = 10L,
               idx    = 1:100L,
               depth  = 20L)

  expect_positive_integer_scalar(".get_indices.data.frame", args, "k",
                                 "`k` must be a positive integer.")

  expect_idx(".get_indices.numeric", args)
})

test_that("`.get_indices()` data.frame S3 method returns expected results", {
  # local implementation
  .local_imp <- function(strata, breaks, k, idx, depth) {
    stratas_1 <- data.frame(idx    = idx,
                            strata = .create_strata(strata[, 1L],
                                                    breaks = breaks[[1L]],
                                                    depth  = depth))
    stratas_1 <- unname(split(stratas_1, stratas_1$strata))

    results <- vector("list", k)
    for ( i in seq_along(stratas_1) ) {
      tmp <- .get_indices(strata[stratas_1[[i]]$idx, 2L],
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
      .get_indices(x, breaks = breaks, k = 4L, idx = 1:1000, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, list(NA, NA), 4L, 1:1000, 5L))
  )

  # data.frame with one continuous and one discrete
  x <- withr::with_seed(42L, data.frame(s1 = stats::runif(1000),
                                        s2 = stats::rbinom(1000, 1, 0.3)))
  # breaks[[1]] is number of bins
  # breaks[[2]] should be ignored -- provide inappropriate value to ensure
  breaks <- list(s1 = 4L, s2 = c(0.25, 0.5, 1.0))
  expect_equal(
    withr::with_seed(1234L,
      .get_indices(x, breaks = breaks, k = 4L, idx = 1:1000, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, list(4, NA), 4L, 1:1000, 5L))
  )

  # breaks[[1]] is a vector of bin boundaries
  # breaks[[2]] should be ignored -- provide an inappropriate value to ensure
  breaks <- list(s1 = c(0.0, 0.5, 1.0), s2 = c(0.25, 0.5, 1.0))
  expect_equal(
    withr::with_seed(1234L,
      .get_indices(x, breaks = breaks, k = 5L, idx = 1:1000, depth = 5L)
    ),
    withr::with_seed(1234L,
      .local_imp(x, breaks = list(breaks[[1L]], NA), k = 5L,
                 idx = 1:1000, depth = 5L)
    )
  )

  # two continuous variables with all combination of breaks
  x <- withr::with_seed(42L, data.frame(s1 = stats::runif(1000),
                                        s2 = stats::runif(1000)))

  breaks <- list(s1 = 4L, s2 = 5L)
  expect_equal(
    withr::with_seed(1234L,
      .get_indices(x, breaks = breaks, k = 4L, idx = 1:1000, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, list(4, 5), 4L, 1:1000, 5L))
  )

  breaks <- list(s1 = c(0.0, 0.6, 1.0), s2 = 5L)
  expect_equal(
    withr::with_seed(1234L,
      .get_indices(x, breaks = breaks, k = 4L, idx = 1:1000, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, list(breaks[[1L]], 5), 4L, 1:1000, 5L))
  )

  breaks <- list(s1 = 4L, s2 = c(0.0, 0.6, 0.8, 1.0))
  expect_equal(
    withr::with_seed(1234L,
      .get_indices(x, breaks = breaks, k = 4L, idx = 1:1000, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, list(4, breaks[[2]]), 4L, 1:1000, 5L))
  )

  breaks <- list(s1 = c(0.0, 0.5, 1.0), s2 = c(0.0, 0.6, 0.8, 1.0))
  expect_equal(
    withr::with_seed(1234L,
      .get_indices(x, breaks = breaks, k = 4L, idx = 1:1000, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, breaks, 4L, 1:1000, 5L))
  )
})

test_that("`.get_indices()` default method returns expected results", {
  folds <- withr::with_seed(42L, sample(rep(1:3L, length.out = 30L)))
  expected <- list(
    (1:30L)[folds == 1L],
    (1:30L)[folds == 2L],
    (1:30L)[folds == 3L]
  )

  expect_equal(
    withr::with_seed(42L, .get_indices(NULL, 3L, 1:30L, NULL)),
    expected
  )

  idx <- withr::with_seed(1234L, sample(1:30L, 30L, TRUE))
  folds <- withr::with_seed(42L, sample(rep(1:4L, length.out = 30L)))
  expected <- list(idx[folds == 1L],
                   idx[folds == 2L],
                   idx[folds == 3L],
                   idx[folds == 4L])

  expect_equal(
    withr::with_seed(42L, .get_indices(NULL, 4L, idx, NULL)),
    expected
  )
})

# .vfold_splits ----
# This function calls `.get_indices()` and manipulates the output.
test_that("`.vfold_splits()` returns expected results", {
  skip("Add specific unit tests for `.vfold_splits()`")
  # No stratification
  expect_equal(
    withr::with_seed(1234L, .vfold_splits(simdata)),
  )

  # stratify on 1 variable with # of bins
  expect_equal(
    withr::with_seed(1234L,
      .vfold_splits(simdata, k = 4L, breaks = list(time = 4))
    )
  )

  # stratify on 1 variable with vector of bins
  breaks_vec <- seq(min(simdata$time), max(simdata$time), length.out = 5)
  expect_equal(
    withr::with_seed(1234L,
      .vfold_splits(simdata, k = 4L, breaks = list(time = breaks_vec))
    )
  )

  # stratify on 2 variable with all combinations of breaks
  expect_equal(
    withr::with_seed(1234L,
      .vfold_splits(simdata, k = 4L, breaks = list(time = 5L, status = 100L))
    )
  )

  expect_equal(
    withr::with_seed(1234L,
      .vfold_splits(simdata, k = 4L, breaks = list(time   = breaks_vec,
                                                   status = NA))
    )
  )
})


# create_kfold ----
test_that("`create_kfold()` returns expected errors", {
  expect_error(
    create_kfold(k = 10L, strata = NULL, breaks = 4L),
    "`data` must be a `data.frame`."
  )
  expect_error(
    create_kfold(1:100),
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
    "`breaks` must be `NULL` or a list of length 1 or 2."
  )
  expect_error(
    create_kfold(simdata, breaks = list("a", "b", "c")),
    "`breaks` must be `NULL` or a list of length 1 or 2."
  )
  expect_error(
    create_kfold(simdata, breaks = NA),
    "`breaks` must be `NULL` or a list of length 1 or 2."
  )
})

test_that("`create_kfold()` returns expected results for `repeats = 1L`", {
  # local implementation
  .local_imp <- function(data, k, breaks, depth) {
    split_obj <- .vfold_splits(data = data, k = k, breaks = breaks, depth = depth)
    split_obj[["repeat"]] <- NA_integer_
    structure(
      list(data = data, splits = split_obj),
      class = c("x_split", "list"),
      breaks = breaks
    )
  }

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
    split_objs <- lapply(set_Names(seq_len(repeats)),
                         function(.i) {
                           tmp <- .vfold_splits(data = data, k = k,
                                                breaks = breaks,
                                                depth = depth)
                           tmp[["repeat"]] <- .i
                           tmp
                         }) |>
      bind_rows() |>
      tibble()
    structure(
      list(data = data, splits = split_objs),
      class = c("x_split", "list"),
      breaks = breaks
    )
  }

  # each input is properly passed along
  expect_equal(
    withr::with_seed(1234L, create_kfold(simdata, k = 5L, repeats = 2L)),
    withr::with_seed(1234L,
      .local_imp(simdata, k = 5L, repeats = 2, breaks = NULL, depth = 20L)
    ),
    ignore_attr = TRUE  # call attribute differs
  )
})


# assessment ----
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

test_that("`assessment()` returns expected values", {
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

test_that("`analysis()` returns expected values", {
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


# S3 print ----
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
test_that("`is.x_split()` returns expected results", {
  obj <- create_kfold(simdata, k = 5L, repeats = 3L,
                      breaks = list(time = 4L, status = NA))
  expect_true(is.x_split(obj))
  expect_false(is.x_split(unclass(obj)))
})
