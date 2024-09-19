
# Setup ----
# Testing ----
# S3 methods
test_that("`makeNumeric()` character class converted when possible", {
  expect_equal(letters, makeNumeric(letters))
  x <- c("0.1", "0.2")
  expect_equal(makeNumeric(x), c(0.1, 0.2))
  expect_equal(makeNumeric(c(x, NA_character_)), c(0.1, 0.2, NA_real_)) # NAs converted
  x <- c(x, "foo")
  expect_equal(makeNumeric(x), x) # non-convertable strings are not
})

test_that("`makeNumeric()` integer class is NOT converted", {
  expect_equal(makeNumeric(1L), 1L)
  expect_equal(makeNumeric(NA_integer_), NA_integer_)
})

test_that("`makeNumeric()` factor class is converted on request", {
  x <- factor(c("A", "B"))
  expect_equal(makeNumeric(x), 1:2)
  expect_equal(makeNumeric(x, coerce.factor = FALSE), x)
})

test_that("`makeNumeric()` logical is converted correctly", {
  expect_equal(makeNumeric(c(TRUE, FALSE)), 1:0)
})

test_that("`makeNumeric()` list is converted correctly", {
  x <- list(a = 1:2L, b = "0.5", c = factor(c("A", "B")), d = TRUE)
  expect_equal(
    makeNumeric(x),
    list(a = 1:2L, b = 0.5, c = 1:2, d = 1)
  )
  expect_equal(
    makeNumeric(x, coerce.factor = FALSE),
    list(a = 1:2L, b = 0.5, c = factor(c("A", "B")), d = 1)
  )
})

# data frame/tibble ----
test_that("`makeNumeric()` data frame method", {
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
  expect_equal(class(makeNumeric(tbl)), class(tbl))
  expect_equal(class(makeNumeric(sim_test_data)), class(sim_test_data))
  tbl2 <- as.data.frame(tbl)
  expect_equal(class(makeNumeric(tbl2)), class(tbl2))

  # only converts the correct columns
  new <- makeNumeric(tbl)
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
  new <- makeNumeric(tbl, coerce.factor = FALSE)
  expect_equal(new$fact, tbl$fact)   # no change
  expect_equal(c(table(vapply(new, class, character(1)))),
               c(character = 1, factor = 1, integer = 1, numeric = 5))

  # test on `getAnalyteInfo()`
  ad <- SomaDataIO::getAnalyteInfo(sample.adat)
  expect_equal(c(table(vapply(ad, class, ""))), c(character = 13, numeric = 3))
})
