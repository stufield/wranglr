skip("Until fix adat control sample adat")

# Setup ----
test <- system.file("extdata", "sample_adat_controls.adat",
                 package = "SomaDataIO", mustWork = TRUE) |>
  SomaDataIO::read_adat()

old <- revertAptNames(test)
tbl <- getAnalyteInfo(test)


# Testing ----
test_that("`revertAptNames()` correctly returns AptNames to old style AptName format", {
  old_aptnames <- readRDS(test_path("0-revert-aptnames.rds"))
  expect_equal(old_aptnames, getAnalytes(old))
  expect_true(is_reverted(old))
})

test_that("`revertAptNames()` takes a `tbl=` param correctly", {
  expect_equal(
    revertAptNames(test),
    revertAptNames(test, tbl)
  )
})

test_that("`convertAptNames()` correctly returns AptNames to new style AptName format", {
  new <- convertAptNames(old)
  expect_equal(getAnalytes(test), getAnalytes(new))
})

test_that("`convertAptNames()` returns same object when already in new format", {
  expect_message(
    foo <- convertAptNames(test),
    "Object is already in the new `seq.XXXX.XX` format"
  )
  expect_equal(foo, test)
})

test_that("`convertAptNames()` returns correct character class method", {
  # mapping maintains order
  expect_equal(convertAptNames(c("ABCD.1234.56", "MMP.9999.88")),
               c("seq.1234.56", "seq.9999.88"))
  # non-seqIds are unmapped
  x <- c("foo", "ABCD.1234.56", "bar")
  expect_equal(
    convertAptNames(x), c("foo", "seq.1234.56", "bar")
  )
  # order doesn't matter
  expect_equal(
    convertAptNames(rev(x)), c("bar", "seq.1234.56", "foo")
  )
})

test_that("`revertAptNames()` dispatches on character class", {
  x <- c("A", "B", "seq.2211.9", "seq.2190.55", "seq.2212.69", "foo")
  expect_equal(
    revertAptNames(x, tbl),
    c("A", "B", "TIMP1.2211.9", "F11.2190.55", "PLAT.2212.69", "foo")
  )
  # reverse order keeps position
  expect_equal(
    revertAptNames(rev(x), tbl),
    c("foo", "PLAT.2212.69", "F11.2190.55", "TIMP1.2211.9", "B", "A")
  )
})

test_that("`revertAptNames()` warning catch missing GeneIds", {
  apts <- head(getAnalytes(test))
  small_tbl <- head(tbl, 4L)
  expect_warning(
    out <- revertAptNames(apts, small_tbl),
    "There were 2 SeqIds that could not be mapped."
  )
  true <- c("HybControlElution.2171.12", "HybControlElution.2178.55",
            "C4A.C4B.2182.54", "F11.2190.55", "NA.2192.63", "NA.2194.91")
  expect_equal(out, true)
})

test_that("`revertAptNames()` corner cases", {
  expect_error(revertAptNames(NA, tbl),
  "Couldn't find a S3 method for this class object: 'logical'")
  expect_error(revertAptNames(data.frame(a = 1), tbl),
  "Couldn't find a S3 method for this class object: 'data.frame'")
  expect_true(is.na(revertAptNames(NA_character_, tbl)))
})
