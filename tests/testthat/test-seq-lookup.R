# Setup ----
x <- withr::with_seed(100, sample(names(sample_df), 2L))

# Testing ----
test_that("`seq_lookup()` returns correct tibble when Aptamers are passed", {
  tbl <- seq_lookup(x, annotations_v4.1)
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

test_that("`seq_lookup()` returns correct tibble with `tbl` is passed", {
  anno <- attr(sample_df, "Col.Meta")
  tbl  <- seq_lookup(x, anno)
  expect_equal(dim(tbl), c(2, 8L))
  expect_equal(tbl$seq, x)
  expect_equal(tbl$SeqId, c("3474-19", "5019-16"))
  expect_equal(tbl$Target, c("Thrombospondin-1", "PGP9.5"))
  expect_equal(tbl$UniProt, c("P07996", "P09936"))
  expect_equal(tbl$EntrezGeneSymbol, c("THBS1", "UCHL1"))
})

test_that("`seq_lookup()` returns correct tibble when SeqIds are passed", {
  x2  <- sub("^seq\\.", "", x) |> sub(pattern = "\\.", replacement = "-")
  tbl <- seq_lookup(x2, annotations_v4.1)
  expect_equal(dim(tbl), c(2, 10L))
  expect_equal(tbl$seq, x2)
  expect_equal(tbl$SeqId, x2)
  expect_equal(tbl$Target, c("Thrombospondin-1", "PGP9.5"))
  expect_equal(tbl$UniProt, c("P07996", "P09936"))
  expect_equal(tbl$EntrezGeneSymbol, c("THBS1", "UCHL1"))
})

test_that("NAs are properly generated for missing features with long seq passed", {
  seqs <- grep("^seq\\.", names(sample_df), value = TRUE)
  tbl <- seq_lookup(seqs, annotations_v4.1)
  expect_equal(dim(tbl), c(length(seqs), 10L))
  expect_equal(sum(is.na(tbl$Target)), 86)
})
