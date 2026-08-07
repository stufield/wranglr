
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
  expect_equal(dim(tbl1), c(4L, 14L))
  expect_equal(dim(tbl2), c(7L, 15L))
})

test_that("the content is correct via snapshots", {
  withr::local_options(list(width = 100L))
  expect_snapshot( tbl1 )
  expect_snapshot( tbl2 )
})


# NA handling ----
test_that("NAs in the summary column are counted and excluded from stats", {
  mt <- mtcars2
  mt$mpg[c(1L, 5L, 10L)] <- NA_real_
  out <- create_sumry_tbl(mt, mpg, cyl)
  # NAs column tallies missing per group
  expect_equal(sum(out$NAs[out$cyl != "Total"]), 3L)
  # total_n counts everything, n counts non-NA
  expect_equal(out$total_n, out$NAs + out$n)
  # Stats computed on non-NA values only
  expect_false(any(is.na(out$mean)))
})

test_that("all-NA summary column yields NA stats but valid structure", {
  mt <- mtcars2
  mt$mpg <- NA_real_
  out <- create_sumry_tbl(mt, mpg, cyl)
  expect_s3_class(out, "tbl_df")
  expect_true(all(out$n == 0L))
  expect_true(all(is.na(out$mean)))
  expect_true(all(is.na(out$sd)))
  expect_true(all(is.na(out$median)))
})


# Total row semantics ----
test_that("the `Total` row equals ungrouped stats on the full data", {
  out <- create_sumry_tbl(mtcars2, mpg, cyl)
  total <- out[out$cyl == "Total", ]
  expect_equal(total$n, nrow(mtcars2))
  expect_equal(total$mean, mean(mtcars2$mpg))
  expect_equal(total$sd,   sd(mtcars2$mpg))
  expect_equal(total$min,  min(mtcars2$mpg))
  expect_equal(total$max,  max(mtcars2$mpg))
  expect_equal(total$median, median(mtcars2$mpg))
})

test_that("the `Total` row is the last row of the output", {
  out <- create_sumry_tbl(mtcars2, mpg, cyl)
  expect_equal(out$cyl[nrow(out)], "Total")
})


# CV column ----
test_that("the CV column equals sd / mean", {
  out <- create_sumry_tbl(mtcars2, mpg, cyl)
  expect_equal(out$CV, out$sd / out$mean)
})


# Grouping structure ----
test_that("group counts sum to the total N", {
  out <- create_sumry_tbl(mtcars2, mpg, cyl)
  group_n <- out$total_n[out$cyl != "Total"]
  total_n <- out$total_n[out$cyl == "Total"]
  expect_equal(sum(group_n), total_n)
})

test_that("multi-grouping produces one row per unique combo plus Total", {
  # mtcars2 has cyl (3 levels) x am (2 levels) = 6 combos; observed = 6
  out <- create_sumry_tbl(mtcars2, mpg, cyl, am)
  # 6 group rows + 1 Total row
  expect_equal(nrow(out), 7L)
  expect_equal(sum(out$cyl == "Total"), 1L)
})


# Failure modes ----
test_that("non-numeric summary column raises an informative error", {
  # min/max/mean not meaningful for factors
  expect_error(
    create_sumry_tbl(mtcars2, cyl, am),
    "not meaningful for factors"
  )
})

test_that("numeric grouping column is coerced to character", {
  # Trade-off of supporting non-factor grouping types: the first
  # grouping column in the output is always character.
  out <- create_sumry_tbl(mtcars, mpg, cyl)
  expect_s3_class(out, "tbl_df")
  expect_type(out$cyl, "character")
  expect_true("Total" %in% out$cyl)
  # Group rows still hold original values as strings
  expect_setequal(setdiff(out$cyl, "Total"), c("4", "6", "8"))
})

test_that("logical grouping column is coerced to character", {
  mt <- mtcars |> mutate(is_v8 = cyl == 8L)
  out <- create_sumry_tbl(mt, mpg, is_v8)
  expect_s3_class(out, "tbl_df")
  expect_type(out$is_v8, "character")
  expect_true("Total" %in% out$is_v8)
  expect_setequal(setdiff(out$is_v8, "Total"), c("TRUE", "FALSE"))
})
