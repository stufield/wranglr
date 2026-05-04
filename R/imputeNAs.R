#' Impute NAs
#'
#' Imputes any `NAs` with the median value.
#'   Must be a numeric data only. `factor` or
#'   `character` class are not allowed.
#'
#' @family impute
#'
#' @param x A `data.frame`, matrix, or numeric vector.
#'
#' @return An object of the same dimension and class as `x`
#'   with any `NA` values replaced with the median of the
#'   corresponding vector.
#'
#' @author Stu Field
#' @seealso [median()]
#'
#' @examples
#' # data frame method
#' df <- select(simdata, id, class_response, seq.2802.68) |> head()
#' df[3, 3] <- NA_real_
#' df
#'
#' imputeNAs(df)
#'
#' # vector method
#' x <- seq(10)
#' x[c(1, 3, 5)] <- NA_real_
#' x
#'
#' median(x, na.rm = TRUE)
#'
#' imputeNAs(x)
#'
#' table(imputeNAs(x)) |> sort() |> tail()
#' @export
imputeNAs <- function(x) UseMethod("imputeNAs")

#' @noRd
#' @importFrom helpr value
#' @export
imputeNAs.default <- function(x) {
  stop(
    "Couldn't find a S3 method for this class object: ",
    value(class(x)), call. = FALSE
  )
}

#' @noRd
#' @export
imputeNAs.character <- function(x) {
  nas <- which(is.na(x))
  if ( length(nas) == 0L ) {
    return(x)
  }
  repl <- sample(na.omit(unique(x)), length(nas), prob = prop.table(table(x)))
  x[nas] <- repl
  x
}

#' @noRd
#' @export
imputeNAs.factor <- function(x) {
  out <- imputeNAs(as.character(x))
  factor(out, levels = levels(x))
}

#' @noRd
#' @export
imputeNAs.data.frame <- function(x) {
  atts <- attributes(x)
  # function to get only cols with NAs & numeric
  .nas <- function(x) is.numeric(x) & any(is.na(x))
  new <- .modify_if(x, .nas, imputeNAs.numeric)
  attributes(new) <- atts
  new
}

#' @noRd
#' @export
imputeNAs.matrix <- function(x) {
  apply(x, 2, imputeNAs)
}

#' @noRd
#' @importFrom stats median
#' @export
imputeNAs.numeric <- function(x) {
  x[is.na(x)] <- median(x, na.rm = TRUE)
  x
}

.modify_if <- function(x, p, f) {
  lgl <- vapply(x, p, NA)
  for ( i in which(lgl) ) {
    x[[i]] <- f(x[[i]])
  }
  x
}
