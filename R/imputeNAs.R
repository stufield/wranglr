#' Impute NAs
#'
#' Imputes any `NAs` with the median value.
#'   Must be a numeric data only. `factor` or
#'   `character` class are not allowed.
#'
#' @family impute
#' @param x A `data.frame`, matrix, or numeric vector.
#' @return An object of the same dimension and class as `x`
#'   with any `NA` values replaced with the median of the
#'   corresponding vector.
#' @author Stu Field
#' @seealso [median()], [modify_if()]
#' @examples
#' adat <- sim_adat
#' imputeNAs(adat)
#'
#' idx <- 25L  # random 25th column
#' imputeNAs(adat[[idx]])  
#'
#' # test the S3 method
#' all.equal(imputeNAs(adat[[idx]]), imputeNAs(adat)[[idx]])
#'
#' x <- adat[[idx]]
#' x[seq(1, nrow(adat), 10L)] <- NA_real_
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
  stop("Cannot impute values for `character` class!",
       call. = FALSE)
}

#' @noRd
#' @export
imputeNAs.factor <- function(x) {
  stop("Cannot impute values for `factor` class!",
       call. = FALSE)
}

#' @noRd
#' @importFrom purrr modify_if
#' @export
imputeNAs.data.frame <- function(x) {
  # function to get only cols with NAs & numerics
  .nas <- function(x) is.numeric(x) & any(is.na(x))
  modify_if(x, .nas, imputeNAs.numeric)
}

#' @noRd
#' @export
imputeNAs.soma_adat <- function(x) {
  atts <- attributes(x)
  new  <- NextMethod()
  attributes(new) <- atts
  new
}

#' @noRd
#' @export
imputeNAs.matrix <- function(x) {
  as.data.frame(x) |>
    imputeNAs() |>
    data.matrix()
}

#' @noRd
#' @importFrom purrr modify_if
#' @importFrom stats median
#' @export
imputeNAs.numeric <- function(x) {
  med <- median(x, na.rm = TRUE)   # nolint: object_usage_linter.
  modify_if(x, is.na, ~ med)
}
