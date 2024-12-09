
# Setup ----
# Testing ----
# S3 methods
test_that("`cast_numeric()` character class converted when possible", {
  expect_equal(letters, cast_numeric(letters))
  x <- c("0.1", "0.2")
  expect_equal(cast_numeric(x), c(0.1, 0.2))
  expect_equal(cast_numeric(c(x, NA_character_)), c(0.1, 0.2, NA_real_)) # NAs converted
  x <- c(x, "foo")
  expect_equal(cast_numeric(x), x) # non-convertable strings are not
})

test_that("`cast_numeric()` integer class is NOT converted", {
  expect_equal(cast_numeric(1L), 1L)
  expect_equal(cast_numeric(NA_integer_), NA_integer_)
})

test_that("`cast_numeric()` factor class is converted on request", {
  x <- factor(c("A", "B"))
  expect_equal(cast_numeric(x), 1:2)
  expect_equal(cast_numeric(x, coerce.factor = FALSE), x)
})

test_that("`cast_numeric()` logical is converted correctly", {
  expect_equal(cast_numeric(c(TRUE, FALSE)), 1:0)
})

test_that("`cast_numeric()` list is converted correctly", {
  x <- list(a = 1:2L, b = "0.5", c = factor(c("A", "B")), d = TRUE)
  expect_equal(
    cast_numeric(x),
    list(a = 1:2L, b = 0.5, c = 1:2, d = 1)
  )
  expect_equal(
    cast_numeric(x, coerce.factor = FALSE),
    list(a = 1:2L, b = 0.5, c = factor(c("A", "B")), d = 1)
  )
})

# data frame/tibble ----
test_that("`cast_numeric()` data frame method", {
  tbl <- withr::with_seed(101, {
    tibble::tibble(
      id     = 1:20,                            # NO
      chr_id = as.character(1:20),              # YES
      logic  = sample(c(TRUE, FALSE), 20, replace = TRUE), # YES
      fact   = factor(letters[1:20L]),           # YES or NO depending on coerce.factor
      num    = rnorm(20),                       # YES
      x      = rep(c("foo", "bar"), each = 10), # NO
      y      = c(as.character(runif(19)), NA_character_),  # YES; only 1 NA, 5% total
      z      = as.character(rnorm(20))          # YES
    )
  })

  # maintains same class
  expect_equal(class(cast_numeric(tbl)), class(tbl))
  expect_equal(class(cast_numeric(simdata)), class(simdata))
  tbl2 <- as.data.frame(tbl)
  expect_equal(class(cast_numeric(tbl2)), class(tbl2))

  # only converts the correct columns
  new <- cast_numeric(tbl)
  expect_named(new, names(tbl))
  expect_equal(new$id, tbl$id)
  expect_equal(new$num, tbl$num)
  expect_equal(new$x, tbl$x)
  expect_equal(new$fact, as.numeric(tbl$fact))       # coerce a factor
  expect_equal(new$chr_id, as.numeric(tbl$chr_id))   # should change
  expect_equal(new$logic, as.numeric(tbl$logic))     # should change
  expect_equal(new$y, as.numeric(tbl$y))             # should change
  expect_equal(new$z, as.numeric(tbl$z))             # should change
  # counts
  expect_equal(c(table(vapply(new, class, ""))),
               c(character = 1, integer = 1, numeric = 6))

  # coerce.factor = FALSE)` is obeyed
  new <- cast_numeric(tbl, coerce.factor = FALSE)
  expect_equal(new$fact, tbl$fact)   # no change
  expect_equal(c(table(vapply(new, class, character(1)))),
               c(character = 1, factor = 1, integer = 1, numeric = 5))
})
