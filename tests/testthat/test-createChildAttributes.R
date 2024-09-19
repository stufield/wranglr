
adat <- sample.adat
cm   <- attributes(adat)$Col.Meta
# starting with "A"
tbl   <- dplyr::filter(cm, grepl("^A", EntrezGeneSymbol))
feats <- seqid2apt(tbl$SeqId)

test_that("`createChildAttributes()` kicks back orignal adat", {
  new <- adat[1:10L, feats]                       # doesn't break attr
  expect_s3_class(attributes(new)$Col.Meta, "tbl_df")  # early check of Col.Meta class
  expect_true(is_intact_attr(new))
  expect_message(
    new <- createChildAttributes(new, adat, verbose = TRUE),
    "Congrats! Looks like `child.df` has GOOD attributes. You're good to go!"
  )
  expect_s3_class(new, "soma_adat")
  expect_true(is_intact_attr(new))
  expect_s3_class(attributes(adat)$Col.Meta, "tbl_df")
  expect_equal(dim(new), c(10L, length(feats)))
})

test_that("`createChildAttributes()` what it is told!", {
  new <- adat[1:10L, c("SampleId", feats)]     # doesn't break attr
  new$foo <- "bar"                             # add meta data field
  attributes(new)$Col.Meta <- NULL             # breaks them
  expect_false(is_intact_attr(new))
  new <- createChildAttributes(new, adat, verbose = FALSE)
  expect_s3_class(new, "soma_adat")
  expect_s3_class(attributes(new)$Col.Meta, "tbl_df")  # Col.Meta class preserved
  expect_true(is_intact_attr(adat))
  expect_true("foo" %in% names(new))
  expect_equal(dim(new), c(10L, length(feats) + 2L)) # feats, SampleId, foo
  expect_equal(attributes(new)$Col.Meta$SeqId,       # check that order Col.Meta
               getSeqId(getAnalytes(new), TRUE))     # is same as in data; re-sync'd
})
