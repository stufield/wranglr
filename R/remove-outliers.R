#' Remove Statistical Outliers
#'
#' Remove statistical outliers from (optionally) _paired_ vectors of numeric
#' values using a statistical criterion based on median absolute deviation
#' (6*mad) and a fold change criterion (5x the median). See `Details` below
#' for more information about the `type =` argument specification. The
#' default is for "`non-parametric`" outlier detection.
#'
#' @param x A numeric vector of RFU values from pair 1
#' @param y Optional. If `NULL`, assume non-paired data and performs outlier
#'   analysis on values in `x`. If _not_ `NULL`, either a numeric vector (RFU)
#'   or character vector (e.g. class names) ordered in the same order as `x`
#'   indicating the pairing.
#' @inherit globalr::get_outliers details
#' @inheritParams globalr::get_outliers
#' @param ... Additional arguments passed to [get_outliers()].
#' @return A `tibble` with columns `x` and `y` representing each
#'   numeric vector pair with statistical outliers removed.
#' @author Amy Zhang, Sophie Weiss, and Stu Field
#' @seealso [get_outliers()]
#' @examples
#' x <- withr::with_seed(101, rnorm(10, mean = 1000, sd = 2))
#' x <- c(x, 10000)          # create outlier (11L)
#' x1 <- remove_outliers(x)   # 'x' only; no 'y'
#' x1
#'
#' y  <- head(LETTERS, length(x))   # paired 'x' and 'y'
#' x2 <- remove_outliers(x, y)       # final row removed
#' x2
#' @importFrom tibble tibble
#' @importFrom globalr is.logspace
#' @export
remove_outliers <- function(x, y = NULL, type = "nonparametric", ...) {

  ret <- tibble(x = x, y = y %||% NA_real_)
  x.  <- if ( is.logspace(x) ) 10^x else x
  idx <- get_outliers(x., type = type, ...)

  if ( !is.null(y) && is.numeric(y) ) {
    y.    <- if ( is.logspace(y) ) 10^y else y
    idx_y <- get_outliers(y., type = type, ...)
    idx   <- union(idx, idx_y)
  }

  if ( length(idx) > 0L ) {
    ret[-idx, ]
  } else {
    ret
  }
}
