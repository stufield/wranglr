skip("This needs fixing")
# Setup ----
x <- withr::with_seed(100, sample(getAnalytes(sample.adat), 2L))

# Testing ----
test_that("`seqLookup()` returns correct tibble when Aptamers are passed", {
  tbl <- seqLookup(x, annotations_v4.1)
  expect_s3_class(tbl, "tbl_df")
  expect_equal(dim(tbl), c(2, 10L))
  expect_named(tbl, c("seq", "SeqId", "EntrezGeneSymbol", "Target", "TargetFullName",
                      "Type", "Dilution", "UniProt", "List", "Reason"))
  expect_equal(tbl$seq, x)
  expect_equal(tbl$SeqId, c("3498-53", "5036-50"))
  expect_equal(tbl$Target, c("IL-17", "TSG-6"))
  expect_equal(tbl$UniProt, c("Q16552", "P98066"))
  expect_equal(tbl$EntrezGeneSymbol, c("IL17A", "TNFAIP6"))
})

test_that("`seqLookup()` returns correct tibble with apt_data is passed", {
  anno <- getAnalyteInfo(sample.adat)
  tbl  <- seqLookup(x, anno)
  expect_equal(dim(tbl), c(2, 8L))
  expect_equal(tbl$seq, x)
  expect_equal(tbl$SeqId, getSeqId(x, TRUE))
  expect_equal(tbl$Target, c("IL-17", "TSG-6"))
  expect_equal(tbl$UniProt, c("Q16552", "P98066"))
  expect_equal(tbl$EntrezGeneSymbol, c("IL17A", "TNFAIP6"))
})

test_that("`seqLookup()` returns correct tibble when SeqIds are passed", {
  x2  <- getSeqId(x, TRUE)
  tbl <- seqLookup(x2, annotations_v4.1)
  expect_equal(dim(tbl), c(2, 10L))
  expect_equal(tbl$seq, x2)
  expect_equal(tbl$SeqId, x2)
  expect_equal(tbl$Target, c("IL-17", "TSG-6"))
  expect_equal(tbl$UniProt, c("Q16552", "P98066"))
  expect_equal(tbl$EntrezGeneSymbol, c("IL17A", "TNFAIP6"))
})

test_that("NAs are properly generated for missing apts with long seq passed", {
  tbl <- seqLookup(getAnalytes(sample.adat), annotations_v4.1)
  expect_equal(dim(tbl), c(getAnalytes(sample.adat, n = TRUE), 10L))
  expect_equal(sum(is.na(tbl$Target)), 86)
})
