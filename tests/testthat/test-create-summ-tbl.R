
# Setup ----
st <- create_summ_tbl(sample.adat)
st_group <- create_summ_tbl(sample.adat, SampleGroup)


# Testing ----
test_that("output is a tibble object", {
  expect_s3_class(st, "tbl_df")
  expect_s3_class(st_group, "tbl_df")
})

test_that("tibble dimensions are correct", {
  expect_s3_class(st, "data.frame")
  expect_s3_class(st_group, "data.frame")
  expect_equal(dim(st), c(1129L, 7L))
  expect_equal(dim(st_group), c(1129L, 13L))
})

test_that("the object names are correct", {
  expect_named(st, c("Feature", "min", "median", "mean", "sd", "mad", "max"))
  expect_named(st_group,
               c("Feature",
                 "min_F", "min_M",
                 "median_F", "median_M",
                 "mean_F", "mean_M",
                 "sd_F", "sd_M",
                 "mad_F", "mad_M",
                 "max_F", "max_M"))
})

test_that("Feature names are correct", {
  expect_equal(st$Feature, get_analytes(sample.adat))
  expect_equal(st_group$Feature, get_analytes(sample.adat))
})

test_that("column values are correct when no `group_var` is passed", {
  expect_equal(st |> dplyr::select(-Feature) |> colSums(),
               c(min    = 5901966.5000000,
                 median = 10965454.6500000,
                 mean   = 11277897.3850000,
                 sd     = 3713840.92289122,
                 mad    = 3451248.31926000,
                 max    = 19664004.50000000))
})

test_that("column values are correct when `group_var` is passed", {
  expect_equal(st_group |> dplyr::select(-Feature) |> colSums(),
               c(min_F    = 6606430.60000000,
                 min_M    = 6561672.40000000,
                 median_F = 11210751.30000000,
                 median_M = 10832407.10000000,
                 mean_F   = 11412855.1818181818,
                 mean_M   = 11112948.96666667,
                 sd_F     = 3478712.63006239,
                 sd_M     = 3671796.56760676,
                 mad_F    = 2763100.715340000,
                 mad_M    = 3376814.08974000,
                 max_F    = 17957002.40000000,
                 max_M    = 17245191.90000000))
})

test_that("the `group_var` can be a unquoted or quoted string", {
  expect_identical(
    st_group,
    create_summ_tbl(sample.adat, "SampleGroup")
  )
})
