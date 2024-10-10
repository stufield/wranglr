#' Rename Elements of a List
#'
#' See [dplyr::rename()], but for lists.
#' Rename list elements simply and efficiently.
#' Missing elements are unchanged and unmatched
#' elements are ignored.
#'
#' @name rename
#' @inheritParams dplyr::rename
#' @param .data A *named* `list` to be renamed.
#' @return A newly named list corresponding to the
#'   expressions passed to the `...`.
#' @examples
#' foo <- list(
#'   a = 1:5,
#'   b = letters[1:10],
#'   c = data.frame(set = 1:4, col = LETTERS[1:4])
#' )
#'
#' # Map all names
#' rename(foo, super = "b", awesome = "a", wicked = "c")
#'
#' # Missing names are un-changed
#' rename(foo, super = "c", wicked = "b")
#'
#' # Extra names are ignored
#' rename(foo, super = "b", wicked = "c", yellow = "d")
#'
#' # The !!! is supported
#' key <- c(super = "b", awesome = "a", wicked = "c")
#' rename(foo, !!! key)
#' @importFrom stats setNames
#' @export
rename.list <- function(.data, ...) {
  if ( is.null(names(.data)) ) {
    stop("The list must be named.", call. = FALSE)
  }
  map <- rlang::list2(...)
  loc <- setNames(match(map, names(.data), nomatch = 0L), names(map))
  loc <- loc[loc > 0L]
  newnames <- names(.data)
  newnames[loc] <- names(loc)
  setNames(.data, newnames)
}
