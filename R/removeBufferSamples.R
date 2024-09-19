#' Remove Buffer Samples From an Object
#'
#' Removes the buffer samples from a `soma_adat`
#' object assuming the `SampleType` field is
#' present in the object and it contains buffer samples.
#' Additional fall-back to the `SampleId` field
#' for older ADATs; otherwise a warning is triggered
#' and no samples are removed. `Buffer` is case-insensitive.
#'
#' @inheritParams imputePredictors
#' @return An object the same class as `data` having
#'   removed rows representing the `Buffer` samples,
#'   i.e. `SampleType == "Buffer"`.
#' @examples
#' df <- withr::with_seed(101, {
#'   data.frame(
#'     PlateId = rep("Set A", 20L),
#'     SampleType = sample(c("Sample", "QC", "Calibrator", "Buffer"),
#'                         size = 10L, replace = TRUE),
#'     clinvar = rnorm(20)
#'   )
#' })
#' table(df$SampleType)
#'
#' removeBufferSamples(df)$SampleType |> table()
#' @export
removeBufferSamples <- function(data) {
  if ( "SampleType" %in% names(data) ) {
    data <- filter(data, tolower(SampleType) != "buffer") # nolint: object_usage_linter.
  } else if ( "SampleId" %in% names(data) ) {
    data <- filter(data, tolower(SampleId) != "buffer") # nolint: object_usage_linter.
  } else {
    warning(
      "Neither `SampleType` nor `SampleId` are ",
      "present in ADAT meta data ... unable to remove ",
      "buffer (no protein) samples as requested.", call. = FALSE
    )
  }
  if ( nrow(data) == 0L ) {
    warning("No rows in ADAT after removing Buffer samples!", call. = FALSE)
  }
  data
}
