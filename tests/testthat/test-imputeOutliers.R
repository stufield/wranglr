
test_that("imputeOutliers() unit test", {
  set.seed(101)
  x <- c(rnorm(26, 15, 2), 4.5, 5, 25, 25.9)
  y <- imputeOutliers(x)
  expect_type(y, "double")
  expect_equal(which(x != y), 27:30)
  expect_equal(sum(x != y), 4)
  expect_equal(sum(y), 441.7901363)
})
