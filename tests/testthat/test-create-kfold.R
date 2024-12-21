
# Setup -----
n <- 100L
mtcars2 <- withr::with_seed(101L, dplyr::sample_n(mtcars, n, replace = TRUE))
mtcars2$disp <- withr::with_seed(101, jitter(mtcars2$disp)) # continuous
mtcars2$vs   <- factor(mtcars2$vs)        # binary 0/1
mtcars2$cyl  <- as.character(mtcars2$cyl) # character 3 levels
split_obj    <- create_kfold(mtcars2, k = 4L)


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
  expect_error(
    .create_strata(withr::with_seed(42L, stats::runif(100)),
                   breaks = c(0.25, 0.5, 0.75)),
    "Provided `breaks` does not span data."
  )
})


test_that("`.create_strata()` returns expected results and warnings", {
  # A discrete variable with 5 or fewer values should return an
  # unmodified character factor vector
  x <- withr::with_seed(1234L, sample(1:5, 100, TRUE))
  expect_equal(.create_strata(x), factor(as.character(x)))

  # numeric vector with depth large enough to change break to >= 2
  x <- withr::with_seed(42L, stats::runif(100))
  expect_error(
    .create_strata(x, breaks = 10, depth = 50L),
    "Too little data to stratify."
  ) |> expect_message("The number of observations in each quantile")

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
  expect_equal(
    .create_strata(x, breaks = c(0.0, 0.0, 0.25, 1.0, 1.0), depth = 1L),
    expected
  )
})


# .get_indices() numeric ----
# calls `.create_strata()` and manipulates the output.
test_that("`.get_indices() numeric S3 method returns expected errors", {
  expect_error(
    .get_indices(1:100, breaks = list(s1 = c(0.0, 0.25, 0.75, 1.0)),
                 k = 5L, depth = 5L),
    "`breaks` must be a vector of cutoffs, not a 'list'"
  )
  args <- list(x      = withr::with_seed(42L, sample(1L:2L, 30L, TRUE)),
               breaks = 4L,
               k      = 10L,
               depth  = 20L)

  expect_positive_integer_scalar(".get_indices.numeric", args, "k",
                                 "`k` must be a positive integer.")
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
     .get_indices(xt, breaks = 2, k = 4L, depth = 5L)
    )
  }

  # continuous variable with number of bins
  x <- withr::with_seed(42L, stats::runif(1000))
  expect_equal(
    withr::with_seed(1234L,
      .get_indices(x, breaks = 4L, k = 5L, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, 4L, 5, 1:1000, 5L))
  )

  # continuous variable with vector of breaks
  x <- withr::with_seed(42L, stats::runif(1000))
  expect_equal(
    withr::with_seed(1234L,
      .get_indices(x, breaks = c(0.0, 0.25, 0.75, 1.0),
                   k = 5L, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, c(0.0, 0.25, 0.75, 1.0), 5, 1:1000, 5L))
  )
})


test_that("`.get_indices()` numeric S3 method returns expected errors", {
  expect_error(
    .get_indices(matrix(1:30, ncol = 1L), breaks = breaks, k = 4L, depth = 2L),
    "`x` must be numeric, not a 'matrix'"
  )
})


# .get_indices() character ----
test_that("`.get_indices()` factor and char S3 method returns expected values", {
  x <- sample(c("a", "d"), 1000, TRUE)
  out <- withr::with_seed(101L,
    .get_indices(x, k = 10L, depth = 20L))
  expect_type(out, "list")
  expect_equal(length(out), 10L)

  # same for factor class
  y <- factor(x)
  expect_equal(
    out,
    withr::with_seed(101L,
      .get_indices(y, k = 10L, depth = 20L))
  )
})


# .get_indices() data.frame ----
test_that("`.get_indices()` data.frame S3 method returns expected errors", {
  strata <- mtcars2[, c("disp", "vs")]
  breaks <- list(disp = 3L, vs = NULL)

  expect_error(
    .get_indices(data.frame(x = 1, Y = 2, Z = 3), breaks = breaks,
                 k = 4L, depth = 2L),
    "`data.frame` must have exactly 2 columns."
  )

  expect_error(
    .get_indices(strata, breaks = breaks[[1L]], k = 4L, depth = 2L),
    "`breaks` must be a list of length 2."
  )

  expect_error(
    .get_indices(strata, breaks = "foo", k = 4L, depth = 2L),
    "`breaks` must be a list of length 2."
  )

  expect_error(
    .get_indices(strata, breaks = list(1, 2, 3), k = 4L, depth = 2L),
    "`breaks` must be a list of length 2."
  )

  expect_error(
    .get_indices(strata, breaks = list(), k = 4L, depth = 2L),
    "`breaks` must be a list of length 2."
  )

  args <- list(x      = strata,
               breaks = breaks,
               k      = 10L,
               depth  = 20L)

  expect_positive_integer_scalar(".get_indices.data.frame", args, "k",
                                 "`k` must be a positive integer.")
})


test_that("`.get_indices()` data.frame S3 method returns expected results", {
  # local implementation
  .local_imp <- function(strata, breaks, k, idx, depth) {
    stratas_1 <- data.frame(
      idx    = idx,
      strata = .create_strata(strata[, 1L], breaks = breaks[[1L]], depth  = depth)
    )
    stratas_1 <- unname(split(stratas_1, stratas_1$strata))
    res <- vector("list", k)
    for ( i in seq_along(stratas_1) ) {
      vec <- strata[stratas_1[[i]]$idx, 2L]
      attr(vec, "idx") <- stratas_1[[i]]$idx
      tmp <- .get_indices(vec, breaks = breaks[[2L]], k = k, depth = depth)
      for ( j in seq_len(k) ) {
        res[[j]] <- c(res[[j]], tmp[[j]])
      }
    }
    res
  }

  n <- 1000L
  # data.frame with two discrete variables both with < 5 levels
  x <- withr::with_seed(42L, data.frame(s1 = stats::rbinom(n, 1, 0.5),
                                        s2 = stats::rbinom(n, 1, 0.3)))
  # breaks should be ignored with early return
  # provide inappropriate value to ensure
  breaks <- list(s1 = "foo", s2 = "bar")

  expect_equal(
    withr::with_seed(1234L,
      .get_indices(x, breaks = breaks, k = 4L, depth = 5L)
    ),
    withr::with_seed(1234L,
      .local_imp(x, breaks, 1:n, k = 4L, depth = 5L)
    )
  )

  # data.frame with 1 continuous and 1 discrete
  x <- withr::with_seed(42L, data.frame(s1 = stats::runif(n),
                                        s2 = stats::rbinom(n, 1, 0.3)))
  # breaks[[1]] is number of bins
  # breaks[[2]] should be ignored -- provide inappropriate value to ensure
  breaks <- list(s1 = 4L, s2 = "foo")

  expect_equal(
    withr::with_seed(1234L,
      .get_indices(x, breaks = breaks, k = 4L, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, breaks, 4L, 1:n, 5L))
  )

  # breaks[[1]] is a vector of bin boundaries
  # breaks[[2]] should be ignored -- provide an inappropriate value to ensure
  breaks <- list(s1 = c(0.0, 0.5, 1.0), s2 = "foo")

  expect_equal(
    withr::with_seed(1234L,
      .get_indices(x, breaks = breaks, k = 5L, depth = 5L)
    ),
    withr::with_seed(1234L,
      .local_imp(x, breaks, k = 5L, 1:n, depth = 5L)
    )
  )

  # two continuous variables with all combination of breaks
  x <- withr::with_seed(42L, data.frame(s1 = stats::runif(n),
                                        s2 = stats::runif(n)))

  breaks <- list(s1 = 4L, s2 = 5L)
  expect_equal(
    withr::with_seed(1234L,
      .get_indices(x, breaks = breaks, k = 4L, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, breaks, 4L, 1:n, 5L))
  )

  breaks <- list(s1 = c(0.0, 0.6, 1.0), s2 = 5L)
  expect_equal(
    withr::with_seed(1234L,
      .get_indices(x, breaks = breaks, k = 4L, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, breaks, 4L, 1:n, 5L))
  )

  breaks <- list(s1 = 4L, s2 = c(0.0, 0.6, 0.8, 1.0))
  expect_equal(
    withr::with_seed(1234L,
      .get_indices(x, breaks = breaks, k = 4L, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, breaks, 4L, 1:n, 5L))
  )

  breaks <- list(s1 = c(0.0, 0.5, 1.0), s2 = c(0.0, 0.6, 0.8, 1.0))
  expect_equal(
    withr::with_seed(1234L,
      .get_indices(x, breaks = breaks, k = 4L, depth = 5L)
    ),
    withr::with_seed(1234L, .local_imp(x, breaks, 4L, 1:n, 5L))
  )
})


# .vfold_splits ----
# called directly by `create_kfold()` n repeats times.
# calls `.get_indices()` and manipulates the output.
test_that("`.vfold_splits()` returns expected output for given inputs", {
  # test with snapshots rather than static output
  # should be easier to create truths and simplify updates
  expect_snapshot(variant = "vfold_splits", {
    # No stratification
    out <- withr::with_seed(101L, .vfold_splits(mtcars2, k = 3L))
    out
    out$split
  })

  expect_snapshot(variant = "vfold_splits", {
    # stratify on disp only with 3 bins
    out <- withr::with_seed(101L,
      .vfold_splits(mtcars2, k = 3L, breaks = list(disp = 3L))
    )
    out
    out$split
  })

  expect_snapshot(variant = "vfold_splits", {
    # stratify on disp with vector of bins
    breaks_vec <- seq(min(mtcars2$disp), max(mtcars2$disp), length.out = 5L)
    out <- withr::with_seed(101L,
      .vfold_splits(mtcars2, k = 3L, breaks = list(disp = breaks_vec))
    )
    out
    out$split
  })

  expect_snapshot(variant = "vfold_splits", {
    # stratify on disp and mpg with combinations of breaks
    # must modify default depth to avoid error (too little data)
    out <- withr::with_seed(101L,
      .vfold_splits(mtcars2, k = 3L, breaks = list(disp = 3L, mpg = 5L),
                    depth = 5L)
    )
    out
    out$split
  })

  expect_snapshot(variant = "vfold_splits", {
    # stratify on disp and mpg with vector of bins and binary vs NULL
    out <- withr::with_seed(101L,
      .vfold_splits(mtcars2, k = 3L, breaks = list(disp = breaks_vec, vs = NULL))
    )
    out
    out$split
  })
})


# create_kfold ----
test_that("`create_kfold()` returns expected errors", {
  expect_error(
    create_kfold(k = 10L, strata = NULL, breaks = 4L),
    "`data` must be a `data.frame`."
  )
  expect_error(
    create_kfold(seq(5)),
    "`data` must be a `data.frame`."
  )
  expect_error(
    create_kfold(letters),
    "`data` must be a `data.frame`."
  )

  expect_positive_integer_scalar("create_kfold",
                                 list(data = mtcars2, k = 10L),
                                 "k",
                                 "`k` must be a positive integer.")

  expect_positive_integer_scalar("create_kfold",
                                 list(data = mtcars2, repeats = 10L),
                                 "repeats",
                                 "`repeats` must be a positive integer.")

  expect_error(
    create_kfold(mtcars2, breaks = list()),
    "`breaks` must be `NULL` or a list of length 1 or 2."
  )
  expect_error(
    create_kfold(mtcars2, breaks = list("a", "b", "c")),
    "`breaks` must be `NULL` or a list of length 1 or 2."
  )
  expect_error(
    create_kfold(mtcars2, breaks = NA),
    "`breaks` must be `NULL` or a list of length 1 or 2."
  )
})


test_that("`create_kfold()` returns expected results for `repeats = 1L`", {
  # local implementation
  .local_imp <- function(data, k, breaks, depth) {
    split_obj <- .vfold_splits(data, k, breaks, depth)
    split_obj$.repeat <- NA_integer_
    structure(
      list(data = data, splits = split_obj),
      class = c("x_split", "list"),
      breaks = breaks
    )
  }

  # each input is properly passed along
  expect_equal(
    withr::with_seed(1234L, create_kfold(mtcars2, k = 5L)),
    withr::with_seed(1234L,
      .local_imp(data = mtcars2, k = 5L, breaks = NULL, depth = 20L)
    ),
    ignore_attr = TRUE   # call attr differs
  )

  expect_equal(
    withr::with_seed(1234L, create_kfold(mtcars2, breaks = list(disp = 3L))),
    withr::with_seed(1234L,
      .local_imp(data = mtcars2, k = 10L, breaks = list(disp = 3L), depth = 10L)
    ),
    ignore_attr = TRUE   # call attr differs
  )

  expect_equal(
    withr::with_seed(1234L, create_kfold(mtcars2, breaks = list(disp = 3L))),
    withr::with_seed(1234L,
      .local_imp(data = mtcars2, k = 10L, breaks = list(disp = 3L), depth = 20L)
    ),
    ignore_attr = TRUE   # call attr differs
  )
})


test_that("`create_kfold()` returns expected results for repeats != 1", {
  # local implementation
  .local_imp <- function(data, k, repeats, breaks, depth) {
    split_objs <- lapply(set_Names(seq_len(repeats)),
                         function(.i) {
                           tmp <- .vfold_splits(data, k, breaks, depth)
                           tmp$.repeat <- .i
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
    withr::with_seed(1234L, create_kfold(mtcars2, k = 5L, repeats = 2L)),
    withr::with_seed(1234L,
      .local_imp(mtcars2, k = 5L, repeats = 2, breaks = NULL, depth = 20L)
    ),
    ignore_attr = TRUE  # call attribute differs
  )
})


# assessment ----
test_that("`assessment()` returns expected errors", {
  expect_error(
    assessment(mtcars2),
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
  expected  <- vector("list", 4L)
  for ( i in seq_len(4L) ) {
    expected[[i]] <- mtcars2[split_obj$splits$split[[i]]$assessment, ]
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
  expect_error(
    analysis(mtcars2),
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
  expected <- vector("list", 4L)
  for ( i in seq_len(4L) ) {
    expected[[i]] <- mtcars2[split_obj$splits$split[[i]]$analysis, ]
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
    create_kfold(mtcars2, k = 4L, repeats = 1L)
  )

  # no stratification; w/ repeats
  expect_snapshot(
    create_kfold(mtcars2, k = 4L, repeats = 3L)
  )

  # no stratification; no repeats; based on a data.frame
  expect_snapshot({
    df <- data.frame(mtcars2[, c("disp", "vs")])
    create_kfold(df, k = 5L)
  })

  # stratification on 1 discrete variable
  expect_snapshot(
    create_kfold(mtcars2, k = 5L, breaks = list(vs = NULL))  # vs => binary
  )

  # stratification on 2 variables; 1 continuous + 1 discrete
  expect_snapshot(
    create_kfold(mtcars2, k = 5L, repeats = 3L, breaks = list(disp = 3L, vs = NULL))
  )
})


# is.x_split ----
test_that("`is.x_split()` returns expected results", {
  expect_true(is.x_split(split_obj))
  expect_false(is.x_split(unclass(split_obj)))
})
