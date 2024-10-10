# Setup ----
N <- 10
x <- withr::with_seed(101, rnorm(N, mean = 1000, sd = 2))
x1 <- c(x, 5500)              # add outlier


# Testing ----
test_that("remove_outliers() gives expected output when y = NULL", {
  out_x <- remove_outliers(x)   # no outliers
  expect_equal(out_x$x, x)     # same

  expect_s3_class(out_x, "tbl_df")
  expect_equal(dim(out_x), c(N, 2))
  expect_named(out_x, c("x", "y"))
  expect_true(all(is.na(out_x$y)))
  expect_type(out_x$x, "double")
  expect_type(out_x$y, "double")
  # with final row outlier
  out_x1 <- remove_outliers(x1)
  expect_identical(out_x1, out_x)  # identical; no need to test above again
})

test_that("remove_outliers() gives expected output when y != NULL (paired)", {
  y <- LETTERS[seq_along(x)]
  out <- remove_outliers(x, y)       # no outliers
  expect_equal(out, tibble(x, y))   # tibble with x & y
  expect_equal(dim(out), c(N, 2))
  expect_named(out, c("x", "y"))
  expect_type(out$x, "double")
  expect_type(out$y, "character")
  # with final row outlier
  y1 <- LETTERS[seq_along(x1)]
  expect_equal(remove_outliers(x1, y1), out)
})
