
test_that("Group Info: group_vars group_labels unit test", {
  # Species => factor
  iris_grouped <- dplyr::group_by(datasets::iris, Species)
  labs <- group_labels(iris_grouped)
  expect_s3_class(labs, "tbl_df")
  expect_s3_class(labs$Species, "factor")
  expect_equal(labs$Species, factor(c("setosa", "versicolor", "virginica")))
  expect_equal(dplyr::n_groups(iris_grouped), nrow(labs))

  # cyl => double
  mt_grouped <- dplyr::group_by(mtcars, cyl)
  labs2 <- group_labels(mt_grouped)
  expect_s3_class(labs2, "tbl_df")
  expect_type(labs2$cyl, "double")
  expect_equal(labs2$cyl, c(4, 6, 8))
  expect_equal(dplyr::n_groups(mt_grouped), nrow(labs2))
})

test_that("Group Info: group_vars group_labels with 2 grouping variables", {
  # 2 grouping cyl & vs; with vs => factor
  df <- mtcars
  df$vs <- factor(df$vs)
  mt_grouped <- dplyr::group_by(df, cyl, vs)
  labs <- group_labels(mt_grouped)
  expect_s3_class(labs, "tbl_df")
  expect_type(labs$cyl, "double")
  expect_s3_class(labs$vs, "factor")
  expect_equal(labs$cyl, c(4, 4, 6, 6, 8))
  expect_equal(labs$vs, factor(c(0, 1, 0, 1, 0)))
  expect_equal(dplyr::n_groups(mt_grouped), nrow(labs))
})
