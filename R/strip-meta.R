#' Strip Meta Data
#'
#' Strips the, non-feature (clinical), meta data from a `data.frame` object
#' and returns a data matrix of only the feature data.
#'
#' @param data A `data.frame` object containing feature data.
#' @param feat A vector of column names corresponding to the features.
#' @return A `data.matrix` object containing only the
#'   data matrix of values for the features.
#' @author Stu Field
#' @seealso [data.matrix()]
#' @examples
#' dim(sim_adat)
#' class(sim_adat)
#'
#' strip_meta(sim_adat) |> dim()
#' strip_meta(sim_adat) |> class()
#' @export
strip_meta <- function(data, feat = NULL) {
  if ( inherits(data, "soma_adat") ) {
    data.matrix(data[, getAnalytes(data)])
  } else if ( !is.null(feat) ) {
    data.matrix(data[, feat])
  } else {
    idx <- vapply(data, is.numeric, NA)
    data.matrix(data[, idx])
  }
}
