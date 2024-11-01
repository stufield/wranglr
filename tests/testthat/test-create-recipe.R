# Setup ----
# Train data ----
n <- 10L
train <- withr::with_seed(101,
  data.frame(
    sample_id  = letters[1:n],
    seq.1212.1 = rnorm(n, mean = 1000, sd = 25),
    seq.2929.5 = rnorm(n, mean = 2000, sd = 25),
    row.names  = LETTERS[1:n]
  )
)

rcp   <- create_recipe(train)
feats <- get_analytes(train)

# Test data ----
nn <- 5L
test <- withr::with_seed(1,
  data.frame(
    sample_id  = letters[1:nn],
    seq.1212.1 = rnorm(nn, mean = 1000, sd = 25),
    seq.2929.5 = rnorm(nn, mean = 2000, sd = 25),
    row.names  = LETTERS[1:nn]
  )
)


# Testing recipes ----
test_that("the controller script generates the correct recipe object", {
  expect_s3_class(rcp, "rcp")
  expect_named(rcp, c("call", "features", "log10_lgl", "center_lgl",
                      "scale_lgl", "n", "p", "par_tbl", "dot_vars"))
  expect_true(rcp$log10_lgl)
  expect_true(rcp$center_lgl)
  expect_true(rcp$scale_lgl)
  expect_equal(rcp$n, n)
  expect_equal(rcp$p, 2L)
  expect_true(.check_par_tbl(rcp$par_tbl))    # check from `center-scale.R`
  expect_equal(rcp$par_tbl$feature, feats)
})

test_that("S3 print method generates the correct snapshot", {
  expect_snapshot(rcp)
})

# Testing bake ----
test_that("you can apply the recipe (bake) to an arbitrary test data set", {
  post <- bake_recipe(rcp, test)
  expect_s3_class(post, "data.frame")
  expect_false(is.baked(test))
  expect_true(is.baked(post))
  true <- data.frame(row.names = c("A", "B", "C", "D", "E"),
    sample_id  = c("a", "b", "c", "d", "e"),
    seq.1212.1 = c(-1.50132850995389, -0.0986207113169268, -1.86820978628103,
                   2.27942098298382, 0.150940818856542),
    seq.2929.5 = c(-0.379690672634024, 0.936024632282191, 1.18597435160141,
                   1.02413258913399, 0.141044085569216)
  )
  expect_equal(data.frame(post), true)
})

test_that("recipe-bake workflow is same as center_scale()", {
  x   <- log10(train[, feats])
  tbl <- tibble::tibble(
    feature = feats,
    means   = unname(colMeans(strip_meta(x))),
    sds     = unname(apply(strip_meta(x), 2, sd))
  )
  bkd <- bake_recipe(rcp, test)
  attr(bkd, "baked") <- NULL    # 'baked' attribute differs; rm to compare
  test2 <- test
  test2[, feats] <- log10(test2[, feats])
  expect_equal(bkd, center_scale(test2, tbl))
})

# Testing convert ----
test_that("you can convert a recipe object to a `rcp` object correctly", {
  rec <- recipes::recipe(sample_id ~ ., data = train) |>
    recipes::step_log(recipes::all_predictors(), base = 10) |>
    recipes::step_center(recipes::all_predictors()) |>
    recipes::step_scale(recipes::all_predictors()) |>
    recipes::prep()

  new <- convert_recipe(rec)
  expect_s3_class(new, "converted_recipe")
  expect_s3_class(new, "rcp")
  # the `call` element will differ
  # test everything else
  expect_equal(new[-1L], rcp[-1L])
})

test_that("you can convert a recipe without log10-transform step", {
  rec <- recipes::recipe(sample_id ~ ., data = train) |>
    recipes::step_center(recipes::all_predictors()) |>
    recipes::step_scale(recipes::all_predictors()) |>
    recipes::prep()

  new <- convert_recipe(rec)
  expect_s3_class(new, "converted_recipe")
  expect_s3_class(new, "rcp")
  expect_equal(new[-1L], create_recipe(train, log10 = FALSE)[-1L])
})

test_that("converting a `rcp` object returns the `rcp` as is", {
  expect_equal(convert_recipe(rcp), rcp)
})
