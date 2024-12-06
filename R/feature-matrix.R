#' Select the Feature Matrix
#'
#' Selects the feature (variables) data from
#'   a `data.frame` object and returns a data matrix
#'   of only the feature (usually numeric) data.
#'   Particularly useful in selecting prior to
#'   [stats::predict()] methods that require
#'   a named, numeric data matrix as input.
#'
#' @param data A `data.frame` object containing
#'   numeric feature data.
#' @param feat `charcter(n)`. A vector of column
#'   names corresponding to the features.
#'
#' @return A numeric `data.matrix` object containing
#'   only the data matrix of values for the features.
#' @author Stu Field
#'
#' @seealso [data.matrix()]
#'
#' @examples
#' dim(sim_adat)
#' class(sim_adat)
#'
#' feature_matrix(sim_adat) |> dim()
#' feature_matrix(sim_adat) |> class()
#' @export
feature_matrix <- function(data, feat = NULL) {
  if ( inherits(data, "soma_adat") ) {
    data.matrix(data[, get_analytes(data)])
  } else if ( !is.null(feat) ) {
    data.matrix(data[, feat])
  } else {
    idx <- vapply(data, is.numeric, NA)
    data.matrix(data[, idx])
  }
}
