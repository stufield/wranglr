#' Impute Predictor Data
#'
#' Threshold RFU values according to the model-specific analyte QC ranges. One
#' form of imputation can be capping RFU values above a threshold (i.e.
#' winsorizing).
#'
#' @family impute
#' @param data A `soma_adat`, `data.frame`, or `tibble` object.
#' @param extrm_vals A tibble (`tbl_df`) object with the following 5 fields:
#' \describe{
#'   \item{`AptName:`}{SOMAmer reagent feature name matching fields in `data`}
#'   \item{`xtrm_min:`}{minimum acceptable value for that feature}
#'   \item{`xtrm_max:`}{maximum acceptable value for that feature}
#'   \item{`impute_min:`}{value to assign if below `xtrm_min`}
#'   \item{`impute_max:`}{value to assign if above `xtrm_max`}
#' }
#' Use `NA` to *not* impute, or use `dplyr::filter()` to remove the entire
#' row if neither `min` nor `max` is desired for a given feature.
#' @author Alex Poole, Stu Field
#' @examples
#' x   <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = c(1.23, 4.56, 7.89))
#' tbl <- tibble::tribble(
#'   ~ AptName,  ~ xtrm_max, ~ impute_max, ~ xtrm_min, ~ impute_min,
#'     "a",         NA,        NA,           NA,         NA,
#'     "b",         5,         5,            0,          1,
#'     "c",         9,         7,            7.1,        7.1
#' )
#' imputePredictors(x, tbl)
#' @importFrom globalr is.logspace
#' @export
imputePredictors <- function(data, extrm_vals) {

  if ( "aptname" %in% names(extrm_vals) ) {
    extrm_vals <- dplyr::rename(extrm_vals, "AptName" = aptname)
    warning(
      "Extreme value table uses `aptname`, ",
      "in the future please use `AptName`.", call. = FALSE
    )
  }

  .check_extrm_vals(extrm_vals)
  common_cols <- intersect(names(data), extrm_vals$AptName)

  if ( length(common_cols) == 0L ) {
    stop(
      "No common features between `data` and `extrm_vals$AptName`.",
      call. = FALSE
    )
  }

  extra_apts <- setdiff(extrm_vals$AptName, common_cols)

  if ( length(extra_apts) > 0L ) {
    warning(
      length(extra_apts), " extreme value aptamers not in data.",
      call. = FALSE
    )
  }

  .impute <- function(vec, impute_vals) {
    # NAs do not return boolean -> no update
    new_vec <- vec
    new_vec[vec > impute_vals$xtrm_max] <- impute_vals$impute_max
    new_vec[vec < impute_vals$xtrm_min] <- impute_vals$impute_min
    new_vec
  }

  for ( col in common_cols ) {
    data[[col]] <- .impute(data[[col]], dplyr::filter(extrm_vals, AptName == col))
  }
  data
}

#' Check the extreme values table properties pre-imputation.
#' @param x the imputation table.
#' @importFrom globalr value
#' @noRd
.check_extrm_vals <- function(x) {
  if ( !inherits(x, "tbl_df") ) {
    stop("The 'extrm_vals' object must be a 'tibble' object.", call. = FALSE)
  }
  tbl_nms <- c("AptName", "xtrm_min", "xtrm_max", "impute_min", "impute_max")
  if ( ncol(x) != 5 ) {
    stop(
      "The 'extrm_vals' tibble must contain these 5 columns: ", value(tbl_nms),
      call. = FALSE
    )
  }
  if ( !setequal(tbl_nms, names(x)) ) {
    missn <- setdiff(tbl_nms, names(x))
    stop(
      "The 'extrm_vals' table has missing column(s): ",
      value(missn), call. = FALSE
    )
  }
  min_lgl <- all(x$impute_min >= x$xtrm_min, na.rm = TRUE)
  max_lgl <- all(x$impute_max <= x$xtrm_max, na.rm = TRUE)
  if ( !(min_lgl && max_lgl) ) {
    stop(
      "All 'impute_min' values must be > 'xtrm_min' AND ",
      "all 'impute_max' values must be < 'xtrm_max'.", call. = FALSE
    )
  }
  invisible(NULL)
}

#' Check the transformation space of the data to be imputed
#'  * log10-transformed
#'  * centered
#'  * scaled
#' @param x the data to be imputed.
#' @importFrom stats sd median
#' @noRd
.check_rfu_space <- function(x) {
  # check `x` IS in log-space
  if ( !is.logspace(x) ) {
    warning(
      "The data passed to `imputePredictors()` has not been log-transformed!\n",
      "   Most imputation tables assume log10-transformed data.", call. = FALSE
    )
  }

  rfu     <- strip_meta(x)
  is_cntr <- sum(colSums(rfu)) < 1
  if ( !is_cntr ) {
    warning(
      "The data passed to `imputePredictors()` has not been centered!\n",
      "   Most imputation tables assume centered data.", call. = FALSE
    )
  }

  # if the median difference to 1.0 is > 0.1 ... not scaled
  is_scale <- abs(stats::median(apply(rfu, 2, stats::sd)) - 1.0) < 0.1
  if ( !is_scale ) {
    warning(
      "The data passed to `imputePredictors()` has not been scaled!\n",
      "   Most imputation tables assume scaled data.", call. = FALSE
    )
  }
  invisible(NULL)
}
