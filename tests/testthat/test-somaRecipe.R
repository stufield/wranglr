
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
) |> dress_adat()

rcp  <- somaRecipe(train)
apts <- getAnalytes(train)

# Test data ----
nn <- 5L
test <- withr::with_seed(1,
  data.frame(
    sample_id  = letters[1:nn],
    seq.1212.1 = rnorm(nn, mean = 1000, sd = 25),
    seq.2929.5 = rnorm(nn, mean = 2000, sd = 25),
    row.names  = LETTERS[1:nn]
  )
) |> dress_adat()


# Testing recipes ----
test_that("the controller script generates the correct recipe object", {
  expect_s3_class(rcp, "soma_recipe")
  expect_named(rcp, c("call", "bridge_lgl", "bridge_ref",
                      "log10_lgl", "log10_cols", "center_lgl",
                      "scale_lgl", "n", "p", "par_tbl", "dot_vars"))
  expect_true(rcp$log10_lgl)
  expect_true(rcp$center_lgl)
  expect_true(rcp$scale_lgl)
  expect_equal(rcp$n, n)
  expect_equal(rcp$p, 2L)
  expect_true(.check_par_tbl(rcp$par_tbl))    # check from `center-scale.R`
  expect_equal(rcp$par_tbl$AptName, apts)
})

test_that("S3 print method generates the correct snapshot", {
  expect_snapshot(rcp)
})

# Testing bake ----
test_that("you can apply the recipe (bake) to an arbitrary test data set", {
  post <- somaBake(rcp, test)
  expect_s3_class(post, "soma_adat")
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

test_that("bake double-logging trips error", {
  logtest <- log10(test)
  expect_warning(
    somaBake(rcp, log10(test)),
    "Double-logging danger! The `data` is already in log-space\\."
  )
})

test_that("SomaScan recipe-bake workflow is same as center_scale()", {
  x   <- log10(train)
  tbl <- tibble::tibble(
    AptName = getAnalytes(x),
    means   = unname(colMeans(strip_meta(x))),
    sds     = unname(apply(strip_meta(x), 2, sd))
  )
  bkd <- somaBake(rcp, test)
  attr(bkd, "baked") <- NULL    # 'baked' attribute differs; rm to compare
  expect_equal(bkd, center_scale(log10(test), tbl))
})

# Testing convert ----
test_that("you can convert a recipe object to a soma_recipe object correctly", {
  rec <- recipes::recipe(sample_id ~ ., data = train) |>
    recipes::step_log(recipes::all_predictors(), base = 10) |>
    recipes::step_center(recipes::all_predictors()) |>
    recipes::step_scale(recipes::all_predictors()) |>
    recipes::prep()

  new <- convertRecipe(rec)
  expect_s3_class(new, "converted_recipe")
  expect_s3_class(new, "soma_recipe")
  # the `call` element will differ
  # test everything else
  expect_equal(new[-1L], rcp[-1L])
})

test_that("you can convert a recipe without log10-transform step", {
  rec <- recipes::recipe(sample_id ~ ., data = train) |>
    recipes::step_center(recipes::all_predictors()) |>
    recipes::step_scale(recipes::all_predictors()) |>
    recipes::prep()

  new <- convertRecipe(rec)
  expect_s3_class(new, "converted_recipe")
  expect_s3_class(new, "soma_recipe")
  expect_equal(new[-1L], somaRecipe(train, log10 = FALSE)[-1L])
})

test_that("converting a `soma_recipe` object returns the `soma_recipe` as is", {
  expect_equal(convertRecipe(rcp), rcp)
})

test_that("`getAnalytes.soma_recipe()` S3 method works", {
  rcp_apts <- getAnalytes(rcp)
  expect_equal(typeof(rcp_apts), "character")
  expect_length(rcp_apts, length(apts))
  expect_equal(rcp_apts, apts)
})
