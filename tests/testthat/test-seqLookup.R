# Setup ----
x <- withr::with_seed(100, sample(names(sample.adat), 2L))

# Testing ----
test_that("`seqLookup()` returns correct tibble when Aptamers are passed", {
  tbl <- seqLookup(x, annotations_v4.1)
  expect_s3_class(tbl, "tbl_df")
  expect_equal(dim(tbl), c(2, 10L))
  expect_named(tbl, c("seq", "SeqId", "EntrezGeneSymbol",
                      "Target", "TargetFullName", "Type",
                      "Dilution", "UniProt", "List", "Reason"))
  expect_equal(tbl$seq, x)
  expect_equal(tbl$SeqId, c("3474-19", "5019-16"))
  expect_equal(tbl$Target, c("Thrombospondin-1", "PGP9.5"))
  expect_equal(tbl$UniProt, c("P07996", "P09936"))
  expect_equal(tbl$EntrezGeneSymbol, c("THBS1", "UCHL1"))
})

test_that("`seqLookup()` returns correct tibble with `tbl` is passed", {
  anno <- attr(sample.adat, "Col.Meta")
  tbl  <- seqLookup(x, anno)
  expect_equal(dim(tbl), c(2, 8L))
  expect_equal(tbl$seq, x)
  expect_equal(tbl$SeqId, c("3474-19", "5019-16"))
  expect_equal(tbl$Target, c("Thrombospondin-1", "PGP9.5"))
  expect_equal(tbl$UniProt, c("P07996", "P09936"))
  expect_equal(tbl$EntrezGeneSymbol, c("THBS1", "UCHL1"))
})

test_that("`seqLookup()` returns correct tibble when SeqIds are passed", {
  x2  <- sub("^seq\\.", "", x) |> sub(pattern = "\\.", replacement = "-")
  tbl <- seqLookup(x2, annotations_v4.1)
  expect_equal(dim(tbl), c(2, 10L))
  expect_equal(tbl$seq, x2)
  expect_equal(tbl$SeqId, x2)
  expect_equal(tbl$Target, c("Thrombospondin-1", "PGP9.5"))
  expect_equal(tbl$UniProt, c("P07996", "P09936"))
  expect_equal(tbl$EntrezGeneSymbol, c("THBS1", "UCHL1"))
})

test_that("NAs are properly generated for missing apts with long seq passed", {
  seqs <- grep("^seq\\.", names(sample.adat), value = TRUE)
  tbl <- seqLookup(seqs, annotations_v4.1)
  expect_equal(dim(tbl), c(length(seqs), 10L))
  expect_equal(sum(is.na(tbl$Target)), 86)
})
