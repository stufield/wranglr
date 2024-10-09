
# Setup ----
n    <- table(mtcars$vs)
up   <- unname(n[which.max(n)])
down <- unname(n[which.min(n)])

# Testing ----
test_that("rebalance() generates expected values with up-sampling", {
  si <- withr::with_seed(1, rebalance(mtcars, "vs", "down"))
  expect_equal(nrow(si), 2 * down)
  expect_equal(c(table(si$vs)), c(`0` = down, `1` = down))
})

test_that("rebalance() generates expected values with down-sampling", {
  si <- withr::with_seed(1, rebalance(mtcars, "vs", "up"))
  expect_equal(nrow(si), 2 * up)
  expect_equal(c(table(si$vs)), c(`0` = up, `1` = up))
})

test_that("the `tidyselect` works for unquoted strings", {
  a <- withr::with_seed(1, rebalance(mtcars, "vs"))
  myvar <- "vs"
  b <- withr::with_seed(1, rebalance(mtcars, myvar))
  c <- withr::with_seed(1, rebalance(mtcars, vs))
  expect_equal(a, b)
  expect_equal(a, c)
})
