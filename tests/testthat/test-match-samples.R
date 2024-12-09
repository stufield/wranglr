# Setup ---------
df    <- head(simdata, 25L)
train <- withr::with_seed(1, sample_n(df, 15))
new   <- withr::with_seed(2, sample_n(df, 15))
intersect(train$SampleId, new$SampleId)    # there are some overlapping
dfs   <- match_samples(train, new)


# Testing --------
test_that("`match_samples()` has expected output", {
  out_og <- match_samples(train, new, idcol = "SampleId")

  # sanity check
  expect_named(out_og, c("train", "new"))
  expect_equal(out_og$train$SampleId,
               c("001", "002", "006", "011", "016", "022", "023"))
  expect_equal(out_og$new$SampleId,
               c("001", "002", "006", "011", "016", "022", "023"))
  expect_equal(out_og$train, new[c(12L, 10L, 3L, 11L, 15L, 14L, 4L), ])
  expect_equal(out_og$new, train[c(4L, 5L, 13L, 6L, 11L, 9L, 15L), ])


  # check dataframe input
  expected <- lapply(out_og, as.data.frame)
  out_df <- match_samples(as.data.frame(train), as.data.frame(new))
  expect_equal(out_df, expected, ignore_attr = TRUE) # names differ

  # check mixture of `data.frame` and `soma_adat`
  foo <- as.data.frame(train)
  expect_equal(match_samples(foo, new),
               list(foo = expected$train, new = out_og$new))
  foo <- as.data.frame(new)
  expect_equal(match_samples(train, foo),
               list(train = out_og$train, foo = expected$new))
})

test_that("`match_samples()` works with different sample ID inputs", {
  # use a different sample ID column
  train$id       <- train$SampleId
  new$id         <- new$SampleId
  train$SampleId <- new$SampleId <- NULL

  out_diffid <- match_samples(train, new, idcol = "id")
  expect_equal(out_diffid$train$id,
               c("001", "002", "006", "011", "016", "022", "023"))
  expect_equal(out_diffid$new$id,
               c("001", "002", "006", "011", "016", "022", "023"))
})

test_that("`match_samples()` throws errors/warnings as appropriate", {
  # no samples in common
  train_diffid <- mutate(train, SampleId = paste0("a", SampleId))

  expect_error(
    match_samples(train_diffid, new),
    "`train_diffid` and `new` have no samples in common")

  # if dfs are not named, error still is legible
  expect_error(
    match_samples(data.frame(SampleId = 1:50), data.frame(SampleId = 51:100)),
    "`data.frame(SampleId = 51:100)` have no samples in common", fixed = TRUE)

  # if not data.frame
  expect_error(
    match_samples(1:50, data.frame(a = 1)),
    "`x` must be a `data.frame`"
  )

  expect_error(
    match_samples(data.frame(a = 1), 25:76),
    "`y` must be a `data.frame`"
  )

  # if `idcol` does not exist in either df
  #   expect an error (1)
  expect_error(
    match_samples(train, new, idcol = "foo_id"),
    "`idcol` must be a variable in `x`"
  )

  # duplicated sample IDs
  expect_error(
    match_samples(bind_rows(train, train), new),
    "Duplicate sample IDs exist"
  )
})
