#' Refactor Ghost Level Meta Data
#'
#' Refactor the meta data fields in a data frame,
#' removing the "ghost" levels that remain after
#' subsetting a factored column in a data frame.
#'
#' @param data A `data.frame` object containing feature and clinical data.
#' @return A `data.frame` object, identical as the
#'   original except the factor levels in factor class columns
#'   have been refactored to remove "ghost levels".
#' @author Stu Field
#' @seealso [factor()], [droplevels()]
#' @examples
#' sim_adat$Sex <- factor(sim_adat$gender)
#' sim_adat$Sex
#'
#' new <- sim_adat[sim_adat$Sex == "M", ]
#' new$Sex           # ghost levels!
#' levels(new$Sex)   # ghost levels!
#'
#' new2 <- refactor_data(new)
#' new2$Sex
#' levels(new2$Sex)
#' @importFrom globalr signal_info value
#' @export
refactor_data <- function(data) {
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
