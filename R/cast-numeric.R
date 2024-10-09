#' Convert Table Entries to Numeric
#'
#' Selectively convert an R object to a numeric vector if
#' doing so does *not* result in a warning. This is
#' typically the case when a `character` string
#' contains a symbol that cannot be coerced cleanly. The following rules
#' apply during conversion:
#' \itemize{
#'   \item characters converted if no warnings are triggered
#'   \item integers are _never_ converted
#'   \item logical vectors _always_ converted to 0 / 1
#'   \item factors are _optionally_ converted to numeric
#' }
#'
#' @param x An object for S3 dispatch. Usually a character,
#'   data frame, tibble, or named list containing convertible columns.
#' @param ... Additional arguments to allow extensibility to S3 methods.
#' @return An object coerced to a numeric if the rules above can be
#'   followed. A data frame, `tibble`, or list returns the same class object
#'   with the function applied to each element (column).
#' @author Stu Field
#' @seealso [as.numeric()], [modify()]
#' @examples
#' tbl <- tibble::tibble(
#'   id     = 1:20,                            # NO
#'   chr_id = as.character(1:20),              # YES
#'   logic  = sample(c(TRUE, FALSE), 20, replace = TRUE), # YES
#'   fact   = factor(letters[1:20]),           # YES or NO depending on coerce.factor
#'   num    = rnorm(20),                       # YES
#'   x      = rep(c("foo", "bar"), each = 10), # NO
#'   y      = c(as.character(runif(19)), NA),  # YES; only 1 NA, 5% total
#'   z      = as.character(rnorm(20))          # YES
#' )
#' cast_numeric(tbl)
#' @export
cast_numeric <- function(x, ...) UseMethod("cast_numeric")

#' @export
cast_numeric.default <- function(x, ...) {
  as.numeric(x, ...)
}

#' @rdname cast_numeric
#' @export
cast_numeric.character <- function(x, ...) {
  na_warn <- logical(1)
  ret <- withCallingHandlers(
    as.numeric(x),
    warning = function(w) {
      na_warn <<- identical(w$message, "NAs introduced by coercion") # nolint
      invokeRestart("muffleWarning")
    }
  )
  if ( na_warn ) {
    x
  } else {
    ret
  }
}

#' @export
cast_numeric.numeric <- function(x, ...) {
  x
}

#' @rdname cast_numeric
#' @param coerce.factor Logical. Should `factor` types be
#'   converted to their corresponding numeric?
#' @export
cast_numeric.factor <- function(x, ..., coerce.factor = TRUE) {
  if ( coerce.factor ) {
    x <- as.numeric(x)
  }
  x
}

#' @rdname cast_numeric
#' @export
cast_numeric.integer <- function(x, ...) {
  x
}

#' @rdname cast_numeric
#' @export
cast_numeric.logical <- function(x, ...) {
  as.numeric(x)
}

#' @rdname cast_numeric
#' @export
cast_numeric.list <- function(x, ..., coerce.factor = TRUE) {
  modify(x, cast_numeric, coerce.factor = coerce.factor)
}

#' @rdname cast_numeric
#' @importFrom purrr modify
#' @export
cast_numeric.data.frame <- function(x, ..., coerce.factor = TRUE) {
  modify(x, cast_numeric, coerce.factor = coerce.factor)
}
