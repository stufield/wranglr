skip("Fix this once samples with buffers sorted")

# This is the full covance adat: 1 buffer, 4 QC, 7 Cals, and 20 samples
a <- system.file("extdata", "sample_adat_controls.adat",
                  package = "SomaDataIO", mustWork = TRUE) |>
 SomaDataIO::read_adat()

test_that("`removeBufferSamples()` doesn't remove enything if no Buffers in SampleType", {
  no_buff <- removeBufferSamples(a)
  expect_equal(nrow(no_buff), nrow(removeBufferSamples(no_buff)))
})

test_that("`removeBufferSamples()` doesn't remove enything if no Buffers in SampleId", {
  # rm SampleType and logic goes SampleType -> SampleId
  no_buff <- removeBufferSamples(dplyr::select(a, -SampleType))
  expect_equal(nrow(no_buff), nrow(removeBufferSamples(no_buff)))
})

test_that("the `removeBufferSamples()` removes samples", {
  no_buff <- removeBufferSamples(a)
  expect_false("Buffer" %in% no_buff$SampleType)
  expect_equal(dim(no_buff), c(31, 1148))
  expect_equal(c(table(no_buff$SampleType)),
               c(Calibrator = 7,
                 QC         = 4,
                 Sample     = 20))
})

test_that("`removeBufferSamples()` trips warning if no SampleType or SampleId", {
  expect_warning(
    no_buff <- removeBufferSamples(dplyr::select(a, -SampleType, -SampleId)),
    "Neither `SampleType` nor `SampleId` are present in ADAT meta data"
    )
  expect_equal(nrow(no_buff), nrow(a))
})

test_that("`removeBufferSamples()` trips warning if all samples removed", {
  a$SampleType <- "Buffer"
  expect_warning(
    no_buff <- removeBufferSamples(a),
    "No rows in ADAT after removing Buffer samples!"
  )
  expect_equal(nrow(no_buff), 0)

  a$SampleType <- "buffer"     # case insensitive 'buffer' vs 'Buffer'
  expect_warning(
    no_buff <- removeBufferSamples(a),
    "No rows in ADAT after removing Buffer samples!"
  )
  expect_equal(nrow(no_buff), 0)
})
