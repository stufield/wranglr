#' Impute Predictor Variables
#'
#' Threshold numeric values in model predictors according to
#'   set of training ranges. Typically this involves
#'   capping values above or below a threshold.
#'
#' @family impute
#'
#' @param data A `data.frame`, or `tibble` object.
#' @param extrm_vals A `tibble` with the following **5** fields:
#'   \describe{
#'     \item{`Feature:`}{feature name matching fields in `data`}
#'     \item{`xtrm_min:`}{minimum acceptable value for that feature}
#'     \item{`xtrm_max:`}{maximum acceptable value for that feature}
#'     \item{`impute_min:`}{value to assign if below `xtrm_min`}
#'     \item{`impute_max:`}{value to assign if above `xtrm_max`}
#'   }
#' Use `NA` to *not* impute, or use `dplyr::filter()` to remove the entire
#'   row if neither `min` nor `max` is desired for a given feature.
#'
#' @author Stu Field
#'
#' @examples
#' x   <- data.frame(a = 1:3L, b = 4:6L, c = 7:9L, d = c(1.23, 4.56, 7.89))
#' tbl <- tibble::tribble(
#'   ~ Feature,  ~ xtrm_max, ~ impute_max, ~ xtrm_min, ~ impute_min,
#'     "a",         NA,        NA,           NA,         NA,
#'     "b",         5,         5,            0,          1,
#'     "c",         9,         7,            7.1,        7.1
#' )
#' impute_predictors(x, tbl)
#' @export
impute_predictors <- function(data, extrm_vals) {

  if ( "feature" %in% names(extrm_vals) ) {
    extrm_vals <- dplyr::rename(extrm_vals, "Feature" = feature)
  }

  .check_extrm_vals(extrm_vals)
  common_cols <- intersect(names(data), extrm_vals$Feature)

  if ( length(common_cols) == 0L ) {
    stop(
      "No common features between `data` and `extrm_vals$Feature`.",
      call. = FALSE
    )
  }

  extra_feats <- setdiff(extrm_vals$Feature, common_cols)

  if ( length(extra_feats) > 0L ) {
    warning(
      length(extra_feats), " extreme value features not in `data`.",
      "They will be ignored.", call. = FALSE
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
    data[[col]] <- .impute(data[[col]], dplyr::filter(extrm_vals, Feature == col))
  }
  data
}

#' Check the extreme values table properties pre-imputation.
#' @param x the imputation table.
#' @importFrom helpr value
#' @noRd
.check_extrm_vals <- function(x) {
  if ( !inherits(x, "tbl_df") ) {
    stop("The `extrm_vals` object must be a `tibble`.", call. = FALSE)
  }
  tbl_nms <- c("Feature", "xtrm_min", "xtrm_max", "impute_min", "impute_max")
  if ( ncol(x) != 5L ) {
    stop(
      "The `extrm_vals` tibble must contain these 5 columns: ", value(tbl_nms),
      call. = FALSE
    )
  }
  if ( !setequal(tbl_nms, names(x)) ) {
    missn <- setdiff(tbl_nms, names(x))
    stop(
      "The `extrm_vals` table has missing column(s): ",
      value(missn), call. = FALSE
    )
  }
  min_lgl <- all(x$impute_min >= x$xtrm_min, na.rm = TRUE)
  max_lgl <- all(x$impute_max <= x$xtrm_max, na.rm = TRUE)
  if ( !(min_lgl && max_lgl) ) {
    stop(
      "All `impute_min` values must be > `xtrm_min` AND ",
      "all `impute_max` values must be < `xtrm_max`.", call. = FALSE
    )
  }
  invisible(NULL)
}
