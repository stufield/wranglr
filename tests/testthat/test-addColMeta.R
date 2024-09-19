
test_that("`addColMeta()` adds new columns to Col.Meta as expected", {
  anno     <- apt_data
  withr::with_seed(1, {
    anno$frodo <- round(rnorm(nrow(anno)), 2L)
    anno$sam <- round(rnorm(nrow(anno)), 2L)
  })
  x    <- addColMeta(sample.adat, anno)
  orig <- attr(sample.adat, "Col.Meta")
  new  <- attr(x, "Col.Meta")
  expect_s3_class(x, "soma_adat")
  expect_s3_class(new, "tbl_df")

  # new cols added
  expect_equal(setdiff(names(new), names(orig)), c("frodo", "sam"))

  # new cols values exact match
  expect_equal(new$frodo, anno$frodo)
  expect_equal(new$sam, anno$sam)

  # AptName from 'anno' is not added
  expect_false("AptName" %in% names(new))
})
