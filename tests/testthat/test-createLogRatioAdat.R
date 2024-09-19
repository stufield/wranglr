
# Setup ----
sample.adat$time <- rep(1:4, 5)         # random time points
sample.adat$pid  <- rep(1:5, each = 4)  # assign random subjects
a1 <- createLogRatioAdat(sample.adat, pid, time)

# remove baseline
expect_message(
  a2 <- createLogRatioAdat(sample.adat, pid, time, TRUE),
  "Removing 5 baseline samples"
)

# Testing ----
test_that("createLogRatioAdat generates correct dimensions", {
  expect_s3_class(a1, "soma_adat")
  expect_true(is_intact_attr(a1))
  expect_equal(dim(a1), dim(sample.adat))
  expect_equal(getMeta(a1), getMeta(sample.adat))
  expect_equal(getAnalytes(a1), getAnalytes(sample.adat))
})

test_that("createLogRatioAdat generates correct values", {
  expect_equal(range(stripMeta(a1)), c(-6.75093550962, 6.81033300629))
  expect_equal(sum(stripMeta(a1)), -375.401843685)
  expect_equal(median(stripMeta(a1)), 0)
})

test_that("createLogRatioAdat rm.baseline argument returns correct dimensions", {
  expect_s3_class(a2, "soma_adat")
  expect_true(is_intact_attr(a2))
  expect_equal(dim(a2), c(15L, 1146L))
  expect_equal(getMeta(a2), getMeta(sample.adat))
  expect_equal(getAnalytes(a2), getAnalytes(sample.adat))
  expect_false(1 %in% a2$time)
})

test_that("createLogRatioAdat rm.baseline argument returns correct values", {
  expect_equal(range(stripMeta(a2)), range(stripMeta(a1)))   # same range for both
  expect_equal(sum(stripMeta(a2)), -375.401843685)   # same sum b/c only rm zeros
  expect_equal(median(stripMeta(a2)), -0.00963727725903)
})
