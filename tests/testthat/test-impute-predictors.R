
# Setup ----
df <- data.frame(a = 1:3L, b = 4:6L, c = 7:9L, d = c(1.23, 4.56, 6.78))
tbl <- tibble::tribble(
  ~ Feature,  ~ xtrm_max, ~ impute_max, ~ xtrm_min, ~ impute_min,
    "a",         NA,        NA,           NA,         NA,
    "b",         5,         5,            0,          1,
    "c",         9,         7,            7.1,        7.1
)
# true result
x  <- data.frame(
  a = c(1L, 2L, 3L),
  b = c(4L, 5L, 5L),
  c = c(7.1, 8, 9),
  d = c(1.23, 4.56, 6.78)
)

# Testing ----
test_that("imputation thresholds only specified values", {
  expect_equal(x, impute_predictors(df, tbl))
})

test_that("catches trips are error/warn when triggered", {
  expect_error(
    impute_predictors(df, matrix(1:12, ncol = 4L)),
    "The `extrm_vals` object must be a `tibble`."
  )

  expect_error(
    impute_predictors(df, dplyr::select(tbl, -impute_min)),  # rm column of tbl
    paste("The `extrm_vals` tibble must contain these 5 columns:",
          "'Feature', 'xtrm_min', 'xtrm_max', 'impute_min', 'impute_max'")
  )

  expect_error(
    impute_predictors(df, dplyr::rename(tbl, FooBar = "Feature")), # bad name
    paste("The `extrm_vals` table has missing column(s): 'Feature'"),
    fixed = TRUE
  )

  expect_no_warning(
    # lowercase ok
    y <- impute_predictors(df, dplyr::rename(tbl, feature = "Feature"))
  )
  expect_equal(x, y)

  tbl2 <- tbl
  tbl2[3L, 3L] <- 9.5
  expect_error(
    impute_predictors(df, tbl2),
    paste("All `impute_min` values must be > `xtrm_min` AND",
          "all `impute_max` values must be < `xtrm_max`.")
  )
})
