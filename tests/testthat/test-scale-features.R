# Setup ----
data <- withr::with_seed(101,
  data.frame(
    seq.1212.1 = rnorm(10, mean = 1000, sd = 25),
    seq.2929.5 = rnorm(10, mean = 2000, sd = 25),
    foo        = runif(10),
    row.names  = LETTERS[1:10L]
  ))
feats <- c("seq.1212.1", "seq.2929.5", "foo")


# Testing ----
test_that("`scale_features()` returns identical adat when scalars are 1.0", {
  ref <- setNames(rep_len(1.0, length(feats)), feats)
  new <- scale_features(data, ref)
  expect_equal(data, new)
})

test_that("specific analytes are scaled with non-1.0 values", {
  ref <- setNames(c(0.75, 1.1, 1.25), feats)
  # re-order puts reference out of order
  ref <- ref[c(2L, 3L, 1L)]
  a <- scale_features(data, ref)
  expect_s3_class(a, class(data))
  expect_equal(a$seq.2929.5, data$seq.2929.5 * 1.10)
  expect_equal(a$seq.1212.1, data$seq.1212.1 * 0.75)
  expect_equal(a$foo, data$foo * 1.25)
})

# Warnings & Errors ----
test_that("extra refernce analytes are ignored with a warning", {
  # extra scaling analytes in reference
  ref <- setNames(rep(1.0, length(feats)), feats)
  ref <- ref[c(2L, 3L, 1L)]
  ref <- c(ref, bar = 1.0)  # add 1 extra variable
  expect_warning(
    new <- scale_features(data, ref),
    "There are extra scaling values (1) in the reference.",
    fixed = TRUE
  )
  expect_equal(data, new)
})

test_that("missing analytes are skipped with a warning", {
  ref <- setNames(rep(1.0, length(feats)), feats)[-2L]
  expect_warning(
    new <- scale_features(data, ref),
    "Missing scalar value for (1) features. They will not be transformed.",
    fixed = TRUE
  )
  expect_equal(data, new)
})

test_that("no matches returns identical object, with a warning", {
  ref <- c(seq.1234.56 = 1.234)
  expect_warning(
    new <- scale_features(data, ref),
    "No matches in `scale_vec` ... returning original data"
  )
  expect_equal(data, new)
})
