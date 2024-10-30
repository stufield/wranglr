#' Impute NAs
#'
#' Imputes any `NAs`, usually in meta data, with the median value.
#' Must be a numeric. Objects of `factor` or `character` class
#' are not allowed.
#'
#' @family impute
#' @param x A `soma_adat` object, a data frame, matrix,
#' or a numeric vector.
#' @return An object of the same dimension and class as `x`
#' with any `NA` values replaced with the median of the
#' corresponding vector.
#' @author Stu Field
#' @seealso [median()], [modify_if()]
#' @examples
#' adat <- sim_adat
#' imputeNAs(adat)   # 'soma_adat' S3
#'
#' imputeNAs(adat$seq.3032.11)  # 'numeric' method; chosen at random
#'
#' # test the S3 method
#' all.equal(imputeNAs(adat$seq.3032.11), imputeNAs(adat)$seq.3032.11)
#'
#' x <- adat$seq.3032.11
#' x[seq(1, nrow(adat), 10)] <- NA_real_
#' x
#'
#' median(x, na.rm = TRUE)
#'
#' imputeNAs(x)
#'
#' table(imputeNAs(x)) |> sort() |> tail()
#' @importFrom stats median
#' @importFrom helpr value
#' @export
imputeNAs <- function(x) UseMethod("imputeNAs")

#' S3 imputeNAs default method
#' @noRd
#' @export
imputeNAs.default <- function(x) {
  stop(
    "Couldn't find a S3 method for this class object: ",
    value(class(x)), call. = FALSE
  )
}

#' S3 imputeNAs method for class character
#' @noRd
#' @export
imputeNAs.character <- function(x) {
  stop("Cannot impute values for `character` class!", call. = FALSE)
}

#' S3 imputeNAs method for class factor
#' @noRd
#' @export
imputeNAs.factor <- function(x) {
  stop("Cannot impute values for `factor` class!", call. = FALSE)
}

#' S3 imputeNAs method for class data frame
#' @noRd
#' @importFrom purrr modify_if
#' @export
imputeNAs.data.frame <- function(x) {
  # function to get only cols with NAs & numerics
  .nas <- function(x) is.numeric(x) & any(is.na(x))
  modify_if(x, .nas, imputeNAs.numeric)
}

#' S3 imputeNAs method for class `soma_adat`
#' @noRd
#' @export
imputeNAs.soma_adat <- function(x) {
  atts <- attributes(x)
  new  <- NextMethod()
  attributes(new) <- atts
  new
}

#' S3 imputeNAs method for class matrix
#' @noRd
#' @export
imputeNAs.matrix <- function(x) {
  as.data.frame(x) |>
    imputeNAs() |>
    data.matrix()
}

#' S3 imputeNAs method for class numeric
#' @noRd
#' @importFrom purrr modify_if
#' @export
imputeNAs.numeric <- function(x) {
  med <- stats::median(x, na.rm = TRUE)   # nolint: object_usage_linter.
  modify_if(x, is.na, ~ med)
}
