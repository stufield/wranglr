
# Setup ----
df <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = c(1.23, 4.56, 6.78))
tbl <- tibble::tribble(
  ~ AptName,  ~ xtrm_max, ~ impute_max, ~ xtrm_min, ~ impute_min,
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
  expect_equal(x, imputePredictors(df, tbl))
})

test_that("catches trips are error/warn when triggered", {
  expect_error(
    imputePredictors(df, matrix(1:12, ncol = 4)),
    "The 'extrm_vals' object must be a 'tibble' object."
  )

  expect_error(
    imputePredictors(df, dplyr::select(tbl, -impute_min)),  # rm column of tbl
    paste("The 'extrm_vals' tibble must contain these 5 columns:",
          "'AptName', 'xtrm_min', 'xtrm_max', 'impute_min', 'impute_max'")
  )

  expect_error(
    imputePredictors(df, dplyr::rename(tbl, AptNames = "AptName")),  # bad name
    paste("The 'extrm_vals' table has missing column(s): 'AptName'"), fixed = TRUE
  )

  expect_warning(
    y <- imputePredictors(df, dplyr::rename(tbl, aptname = "AptName")), # lowercase
    "Extreme value table uses `aptname`, in the future please use `AptName`."
  )
  expect_equal(x, y)

  tbl2 <- tbl
  tbl2[3L, 3L] <- 9.5
  expect_error(
    imputePredictors(df, tbl2),
    paste("All 'impute_min' values must be > 'xtrm_min' AND",
          "all 'impute_max' values must be < 'xtrm_max'.")
  )
})

test_that("trips warnings when RFU space may be incorrect", {
  # all 3 warnings
  .check_rfu_space(sample.adat) |>
    expect_warning("log-transformed!") |>
    expect_warning("centered!") |>
    expect_warning("scaled!")

  # not centered; not scaled
  .check_rfu_space(log10(sample.adat)) |>
    expect_warning("centered!") |>
    expect_warning("scaled!")

  expect_warning(
    .check_rfu_space(center_scale(sample.adat, scale = FALSE)),
    "Most imputation tables assume scaled data"
  )

  expect_warning(
    .check_rfu_space(center_scale(sample.adat, center = FALSE)),
    "Most imputation tables assume centered data"
  )
})
