skip("Fix with non-adat testing examples")

# Setup ----
adat <- splyr::sim_test_data
apts <- withr::with_seed(101, sample(getAnalytes(adat), 3L))
short_adat <- adat[, c(getMeta(adat), apts)] |> head(3L)


# Testing ----
test_that("`scale_features()` returns identical adat when scalars are 1.0", {
  ref <- setNames(rep(1.0, length(apts)), apts)
  a <- scale_features(short_adat, ref)
  expect_equal(short_adat, a)
})

test_that("specific analytes are scaled with non-1.0 values", {
  ref <- setNames(c(0.75, 1.1, 1.25), getAnalytes(short_adat))
  # re-order puts reference out of order
  # ensures SeqId matching
  ref <- ref[c(2L, 3L, 1L)]
  a <- scale_features(short_adat, ref)
  expect_s3_class(a, "soma_adat")
  expect_equal(a$seq.4487.88, short_adat$seq.4487.88 * 0.75)
  expect_equal(a$seq.4461.56, short_adat$seq.4461.56 * 1.10)
  expect_equal(a$seq.3032.11, short_adat$seq.3032.11 * 1.25)
})

# Warnings & Errors ----
test_that("extra refernce analytes are ignored with a warning", {
  # extra scaling analytes in reference
  ref <- setNames(rep(1.0, length(apts)), apts)
  # ensure SeqId matching
  ref <- ref[c(2, 3, 1L)]
  ref <- c(ref, "1234-56" = 1.0)  # add 1 extra scalar
  expect_warning(
    a <- scale_features(short_adat, ref),
    "There are extra scaling values (1) in the reference.",
    fixed = TRUE
  )
  expect_equal(short_adat, a)
})

test_that("missing analytes are skipped with a warning", {
  ref <- setNames(c(1.0, 1.0), getAnalytes(short_adat)[-2L])
  expect_warning(
    b <- scale_features(short_adat, ref),
    "Missing scalar value for (16) features. They will not be transformed.",
    fixed = TRUE
  )
  expect_equal(short_adat, b)
})

test_that("no matches returns identical object, with a 1 message & 2 warnings", {
  ref <- c("1234-56" = 1.234)
  expect_snapshot( new <- scale_features(short_adat, ref) )
  expect_equal(short_adat, new)
})
