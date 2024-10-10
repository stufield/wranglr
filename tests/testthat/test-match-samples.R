# Setup ---------
df    <- head(sim_test_data, 25L)
train <- withr::with_seed(1, sample_n(df, 15))
new   <- withr::with_seed(2, sample_n(df, 15))
intersect(train$SampleId, new$SampleId)    # there are some overlapping
dfs   <- match_samples(train, new)


# Testing --------
test_that("`match_samples()` has expected output", {
  out_og <- match_samples(train, new, idcol = "SampleId")

  # sanity check
  expect_named(out_og, c("x", "y"))
  expect_equal(out_og$x$SampleId,
               c("001", "002", "006", "011", "016", "022", "023"))
  expect_equal(out_og$y$SampleId,
               c("001", "002", "006", "011", "016", "022", "023"))
  expect_equal(out_og$y, new[c(12L, 10L, 3L, 11L, 15L, 14L, 4L), ])
  expect_equal(out_og$x, train[c(4L, 5L, 13L, 6L, 11L, 9L, 15L), ])


  # check dataframe input
  expected <- lapply(out_og, as.data.frame)
  out_df <- match_samples(as.data.frame(train), as.data.frame(new))
  expect_equal(out_df, expected)

  # check mixture of data.frame and soma_adat
  expect_equal(match_samples(as.data.frame(train), new),
               list(x = expected$x, y = out_og$y))
  expect_equal(match_samples(train, as.data.frame(new)),
               list(x = out_og$x, y = expected$y))
})

test_that("`match_samples()` works with different sample ID inputs", {
  # use a different sample ID column
  train$id       <- train$SampleId
  new$id         <- new$SampleId
  train$SampleId <- new$SampleId <- NULL

  out_diffid <- match_samples(train, new, idcol = "id")
  expect_equal(out_diffid$x$id,
               c("001", "002", "006", "011", "016", "022", "023"))
  expect_equal(out_diffid$y$id,
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
  expect_error(match_samples(1:50, 25:76), "is.data.frame(x) is not TRUE",
               fixed = TRUE)

  # idcol does not exist in dfs
  expect_warning( # expect both error and warning
    expect_error(
      match_samples(train, new, idcol = "sample_id"),
      "`train` and `new` have no samples in common with idcol"
    ),
    "Unknown or uninitialised column: 'sample_id'"
  )

  # duplicated sample IDs
  expect_error(match_samples(bind_rows(train, train), new),
               "Duplicate sample IDs exist")
})
