
x <- withr::with_seed(101, sample(c("fat", "moose", "cat"), 10L, replace = TRUE))
x <- c(x, NA_character_)

test_that("the `impute_median()` charcter method returns correct value", {
  expect_equal(
    withr::with_seed(1, impute_median(x)),
    c(x[-11L], "moose")   # chooses 'moose'
  )
})

test_that("the `impute_median()` factor method errors out", {
  y <- factor(x)
  expect_equal(
    withr::with_seed(1, impute_median(y)),
    c(y[-11L], factor("moose"))   # chooses 'moose'
  )
})

test_that("the `impute_median()` numeric method is correct", {
  withr::with_seed(101, {
    x    <- rnorm(100)
    NAs  <- sample(length(x), 5L)
  })
  x[NAs] <- NA
  x_med  <- median(x, na.rm = TRUE)
  y      <- impute_median(x)
  expect_type(x, "double")
  expect_type(y, "double")
  expect_length(x, 100L)
  expect_length(y, 100L)
  expect_equal(mean(is.na(x)), 0.05)
  expect_equal(sum(y[ NAs ] == x_med), 5)
  expect_equal(y[ NAs ], rep(x_med, 5))
  expect_type(median(x), "double")
  expect_equal(median(x), as.numeric(NA))
  expect_equal(median(x, na.rm = TRUE), x_med)
  expect_equal(median(y), 0.0584654978495017)
  expect_equal(all(y == x), NA)
  expect_true(all(y == x, na.rm = TRUE))
})

test_that("the `impute_median()` for a `soma_adat` returns same attrs", {
  n    <- 150L
  vals <- sample_df[[n]]
  z    <- impute_median(sample_df)
  expect_equal(z, sample_df)                  # check nothing changed; no NAs
  expect_length(z[[n]], nrow(sample_df))
  expect_equal(impute_median(sample_df[[n]]), z[[n]]) # nothing changed
  expect_equal(impute_median(sample_df[[n]]), vals)   # nothing changed
})

test_that("the `impute_median()` data.frame method is correct", {
  x <- matrix(1:100, ncol = 10L)
  diag(x) <- NA
  x <- data.frame(x)
  expect_s3_class(x, "data.frame")
  expect_equal(sum(x, na.rm = TRUE), 4545)   # pre-impute
  y <- impute_median(x)
  expect_s3_class(y, "data.frame")
  expect_equal(sum(y), 5050)                 # post-impute
  expect_equal(data.matrix(y) |> diag(),
               c(6, 16, 26, 36, 46, 55, 65, 75, 85, 95))  # check diag
})

test_that("the `impute_median()` matrix method is correct", {
  x <- matrix(1:100, ncol = 10L)
  expect_true(is.matrix(x))
  diag(x) <- NA
  expect_equal(sum(x, na.rm = TRUE), 4545)   # pre-impute
  y <- impute_median(x)
  expect_true(is.matrix(y))
  expect_equal(sum(y), 5050)                 # post-impute
  expect_equal(diag(y), c(6, 16, 26, 36, 46, 55, 65, 75, 85, 95))  # check diag
})
