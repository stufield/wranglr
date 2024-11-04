
test_that("the `imputeNAs` charcter method errors out", {
  expect_error(imputeNAs(letters), "Cannot")
})


test_that("the `imputeNAs` factor method errors out", {
  expect_error(imputeNAs(factor(letters)), "Cannot")
})


test_that("the `imputeNAs` numeric method is correct", {
  set.seed(101)
  x      <- rnorm(100)
  NAs    <- sample(length(x), 5)
  x[NAs] <- NA
  x_med  <- median(x, na.rm = TRUE)
  y      <- imputeNAs(x)
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


test_that("the `imputeNAs.soma_adat()` method is correct", {
  n        <- 150L
  apt_vals <- sample.adat[[n]]
  z        <- imputeNAs(sample.adat)
  expect_equal(z, sample.adat)                  # check nothing changed; no NAs
  expect_length(z[[n]], nrow(sample.adat))
  expect_equal(imputeNAs(sample.adat[[n]]), z[[n]])    # nothing changed
  expect_equal(imputeNAs(sample.adat[[n]]), apt_vals)  # nothing changed
})


test_that("the `imputeNAs` data.frame method is correct", {
  x <- matrix(1:100, ncol = 10)
  diag(x) <- NA
  x <- data.frame(x)
  expect_s3_class(x, "data.frame")
  expect_equal(sum(x, na.rm = TRUE), 4545)   # pre-impute
  y <- imputeNAs(x)
  expect_s3_class(y, "data.frame")
  expect_equal(sum(y), 5050)                 # post-impute
  expect_equal(data.matrix(y) |> diag(),
               c(6, 16, 26, 36, 46, 55, 65, 75, 85, 95))  # check diag
})


test_that("the `imputeNAs` matrix method is correct", {
  x <- matrix(1:100, ncol = 10)
  expect_true(is.matrix(x))
  diag(x) <- NA
  expect_equal(sum(x, na.rm = TRUE), 4545)   # pre-impute
  y <- imputeNAs(x)
  expect_true(is.matrix(y))
  expect_equal(sum(y), 5050)                 # post-impute
  expect_equal(diag(y), c(6, 16, 26, 36, 46, 55, 65, 75, 85, 95))  # check diag
})
