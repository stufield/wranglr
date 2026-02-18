
# Setup ----
mtcars2 <- mtcars |>
  mutate( cyl = as.factor(cyl), am = as.factor(am) )
tbl1 <- create_sumry_tbl(mtcars2, mpg, cyl)
tbl2 <- create_sumry_tbl(mtcars2, mpg, cyl, am)


# Testing ----
test_that("you must pass a summary column in `data`: error", {
  expect_error(
    create_sumry_tbl(mtcars),
    "You must provide a grouping column in `data`"
  )
  expect_error(
    create_sumry_tbl(mtcars, mpg),
    "You must provide a grouping column in `data`"
  )
})

test_that("output is a tibble object", {
  expect_s3_class(tbl1, "tbl_df")
  expect_s3_class(tbl2, "tbl_df")
})

test_that("tibble dimensions are correct", {
  expect_equal(dim(tbl1), c(4L, 12L))
  expect_equal(dim(tbl2), c(7L, 13L))
})

test_that("the content is correct via snapshots", {
  expect_snapshot( tbl1 )
  expect_snapshot( tbl2 )
})
