skip("This needs fixing")
n <- 20
train <- withr::with_seed(48973,
                          data.frame(
                            sample_id  = letters[1:n],
                            seq.1212.1 = rnorm(n, mean = 1000, sd = 25),
                            seq.2929.5 = rnorm(n, mean = 2000, sd = 25),
                            row.names  = LETTERS[1:n]
                          )
) |> dress_adat()



apts <- getAnalytes(train)

rec <-  recipes::recipe(~ ., data = dplyr::select(train,  -sample_id)) |>
  recipes::step_log(recipes::all_predictors(), base = 10) |>
  recipes::step_center(recipes::all_predictors()) |>
  recipes::step_scale(recipes::all_predictors()) |>
  recipes::prep(training = train)


soma_rec <- splyr::somaRecipe(train)

test_that("`get_recipe_params()` returns sds when `param = 'scale'`", {
  sds_manual <- apply(as.matrix(train[, apts]), 2, function(col) {
    sd(log10(col))
  })

  expect_silent(sds_rec <- get_recipe_params(rec, "scale"))
  expect_silent(sds_soma <- get_recipe_params(soma_rec, "scale"))

  expect_named(sds_rec, apts)
  expect_vector(sds_rec, ptype = numeric(), size = length(apts))
  expect_equal(sds_rec, sds_manual)
  expect_equal(sds_soma, sds_soma)
})



test_that("`get_recipe_params()` returns means when `param = 'center'`", {
  means_manual <- apply(as.matrix(train[apts]), 2, function(col) {
    mean(log10(col))
  })

  expect_silent(means_rec <- get_recipe_params(rec, "center"))
  expect_silent(means_soma <- get_recipe_params(soma_rec, "center"))

  expect_named(means_rec, apts)
  expect_vector(means_rec, ptype = numeric(), size = length(apts))

  expect_equal(means_manual, means_rec)
  expect_equal(means_manual, means_soma)

})

test_that("`get_recipe_params()` gives errors/warnings/info based on input", {
  expect_error(
    get_recipe_params(rec, "mean"),
    "Method not implemented for param 'mean'")

  rec_nostep <- recipes::recipe(~ ., data = dplyr::select(train, -sample_id)) |>
    recipes::step_log(recipes::all_predictors(), base = 10)

  expect_warning(
    out <- get_recipe_params(rec_nostep, "center"),
    "'recipe' has no 'center' step"
  )
  expect_null(out)

  expect_error(get_recipe_params(rec), 'argument "param" is missing')
  expect_error(get_recipe_params(train, "center"), "Couldn't find a S3 method")
})
