#' Strip Meta Data
#'
#' Strips the (clinical) meta data from a `soma_adat` object
#' and returns a data matrix of only the RFU data.
#'
#' @param data A `soma_adat` object containing RFU feature data.
#' @return A `data.matrix` object containing only the
#'   data matrix of RFU values for the features.
#' @author Stu Field
#' @seealso [data.matrix()]
#' @examples
#' dim(sim_test_data)
#' class(sim_test_data)
#'
#' strip_meta(sim_test_data) |> dim()
#' strip_meta(sim_test_data) |> class()
#' @export
strip_meta <- function(data) {
  if ( inherits(data, "soma_adat") ) {
    data.matrix(data[, getAnalytes(data)])
  } else {
    idx <- vapply(data, is.numeric, NA)
    data.matrix(data[, idx])
  }
}
