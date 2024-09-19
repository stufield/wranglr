
foo <- list(
  a = 1:5,
  b = letters[1:10L],
  c = data.frame(set = 1:4, col = LETTERS[1:4L])
)

test_that("the rename.list method error catch works", {
  # when list is un-named
  err <- list(1:5, letters, data.frame(set = 1:4, col = LETTERS[1:4]))
  expect_error(
    rename(err, b = "super", a = "awesome", c = "wicked"),
    "The list must be named."
  )
})

test_that("the `rename.list()` method generates correct names", {
  bar <- rename(foo, super = "b", awesome = "a", wicked = "c")
  expect_type(bar, "list")
  expect_named(bar, c("awesome", "super", "wicked"))
})

test_that("extra and missing names are handled correctly", {
  # a is missing; d is extra
  bar <- rename(foo, super = "b", wicked = "c", yellow = "d")
  expect_type(bar, "list")
  expect_named(bar, c("a", "super", "wicked"))
})

test_that("the `!!!` is handled correctly", {
  key <- c(super = "b", awesome = "a", wicked = "c", blah = "d")
  bar <- rename(foo, !!!key)
  expect_type(bar, "list")
  expect_named(bar, c("awesome", "super", "wicked"))
})
