#' Refactor Ghost Level Meta Data
#'
#' Refactor the meta data fields in a data frame,
#' removing the "ghost" levels that remain after
#' subsetting a factored column in a data frame.
#'
#' @param data A `soma_adat` object containing RFU and meta data.
#' @return A `soma_adat` object, identical as the
#'   original except the factor levels in factor class columns
#'   have been refactored to remove "ghost levels".
#' @author Stu Field
#' @seealso [getMeta()], [factor()], [droplevels()]
#' @examples
#' sim_test_data$Sex <- factor(sim_test_data$gender)
#' sim_test_data$Sex
#'
#' new <- sim_test_data[sim_test_data$Sex == "M", ]
#' new$Sex           # ghost levels!
#' levels(new$Sex)   # ghost levels!
#'
#' new2 <- refactorData(new)
#' new2$Sex
#' levels(new2$Sex)
#' @importFrom globalr signal_info value
#' @export
refactorData <- function(data) {
  lgl <- vapply(data[ getMeta(data)], is.factor, NA, USE.NAMES = TRUE)
  nms <- names(lgl[lgl])
  for ( meta in nms ) {
    levs <- levels(data[[meta]])
    data[[meta]] <- droplevels(data[[meta]])
    sdiff <- setdiff(levs, levels(data[[meta]]))
    if ( length(sdiff) > 0L && interactive() ) {
      signal_info(
        paste("Dropping levels", value(sdiff), "from", value(meta))
      )
    }
  }
  data
}
