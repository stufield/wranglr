#' Add Target Information to Data Table
#'
#' This function takes a tibble of existing column meta data,
#' created with [getAnalyteInfo()], and adds the following:
#' \itemize{
#'   \item `Target`
#'   \item `EntrezGeneSymbol`
#'   \item `SomaId`
#'   \item `UniProt`
#' }
#' to a data frame (e.g. a `stat.table` of statistical results) which
#' contains `AptName`s as rownames. The rownames will be used to match
#' `SeqIds` when joining the two tables, see [left_join()].
#'
#' @param df A data frame object (e.g. the `stat.table` of univariate
#'   KS-tests) with `Aptamer` row names.
#' @param apt.data A `tibble` containing analyte specific annotation data
#'   and/or SomaScan run data (e.g. `AptName`, `Dilution`, `Target`,
#'   `SomaId`, `ColCheck`, `CalReference`, etc.), see [getAnalyteInfo()].
#' @param add.fields Additional fields to include from the `apt.data` object.
#' @return A data frame with original columns of `df`, plus these
#' additional fields:
#'   * "Target"
#'   * "EntrezGeneSymbol"
#'   * "SomaId"
#'   * "UniProt"
#'   * any fields specified by `add.fields`
#'
#' merged by `AptName` as leading columns in the returned data frame.
#' @author Stu Field
#' @seealso [getAnalyteInfo()], [read_adat()], [left_join()]
#' @examples
#' anno <- SomaDataIO::getAnalyteInfo(sim_test_data)
#'
#' # Generate dummy `df` object; e.g. `stat.table`
#' df <- withr::with_seed(1, {
#'   data.frame(
#'     statistic = rnorm(10),
#'     p.value   = runif(10),
#'     rank      = 1:10,
#'     row.names = sample(anno$AptName, 10)    # important; merge by field
#'   )
#' })
#' df
#'
#' # AptNames of interest (10)
#' dplyr::filter(anno, AptName %in% rownames(df))
#'
#' # Join tables with Annotations (`anno`)
#' addTargetInfo(df, apt.data = anno)
#' @importFrom purrr modify_if
#' @importFrom tibble as_tibble
#' @importFrom SomaDataIO col2rn getSeqIdMatches rn2col
#' @export
addTargetInfo <- function(df, apt.data, add.fields = NULL) {

  if ( missing(apt.data) ) {
    stop("Must pass an `apt.data =` argument to `addTargetInfo()`",
         call. = FALSE)
  }

  ad <- data.frame(apt.data)        # if grouped_df -> nuke groups

  if ( !setequal(ad$AptName, rownames(df)) ) {
    # this is for safety when names differ
    m  <- getSeqIdMatches(ad$AptName, rownames(df)) # in 'ad' order
    ad <- dplyr::filter(ad, AptName %in% m[[1L]])   # subset ad
    ad$AptName <- m[[2L]]                           # overwrite AptName
  }

  add <- c("AptName", "Target", "TargetFullName",
           "EntrezGeneSymbol", "SomaId", "UniProt")

  if ( !is.null(add.fields) ) {
    add <- union(add, add.fields)
  }

  new_cols <- intersect(add, names(ad))

  ad <- ad[, new_cols, drop = FALSE] |>
    modify_if(is.character, ~ gsub(",", ";", .x)) # zap commas

  df  <- rn2col(df, "AptName")             # add rn -> idx column for merge
  res <- left_join(df, ad, by = "AptName") # merge in target into from ad
  col2rn(res[, union(names(ad), names(df))], "AptName") # re-order and put back rn
}
