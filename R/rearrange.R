#' Rearrange Rows by Variables
#'
#' Rearrange (reorder) and possibly filter the rows
#'   of a data frame or tibble object according to a
#'   matched vector. Similar to [dplyr::filter()]
#'   and [dplyr::arrange()], however the
#'   ordering is based on an external object `x`,
#'   rather than a variable contained in `tbl`.
#'
#' This function is meant to act as a working solution to
#'   the lack of row names inherent to tibble objects, yet
#'   still plays nice with `tidyverse` syntax.
#'
#'   Values in `x` that are not in `tbl[[var]]`
#'   are silently dropped. Values in `tbl[[var]]` that are
#'   not in `x` are silently filtered.
#'
#'   Duplicates in `x` are silently dropped to avoid
#'   creating extra rows in `tbl`, whereas duplicates
#'   in `var` are maintained.
#'
#' @param tbl A data frame or tibble object.
#' @param var Character. The variable name (column).
#' @param x A vector of values used to sort and match.
#'
#' @return A reordered, and possibly filtered, object of the
#'   same class as `tbl`.
#' @author Stu Field
#'
#' @seealso [match()]
#'
#' @examples
#' df <- tibble::tibble(name = letters[1:4L], x = 1:4, y = rnorm(4))
#' df
#'
#' rearrange(df, "name", c("c", "b", "d"))
#'
#' rearrange(df, "name", "d")
#'
#' rearrange(df, "name", c("a", "c", "z"))  # "z" is ignored
#'
#' rearrange(df, "name", c("a", "c", "c"))  # duplicate "c" is dropped
#' @export
rearrange <- function(tbl, var, x) {
  stopifnot(
    inherits(tbl, "data.frame"),
    var %in% names(tbl)
  )
  .get_idx <- function(.x) which(match(tbl[[var]], .x, nomatch = 0) > 0)
  ids <- lapply(unique(x), .get_idx)
  tbl[unlist(ids), ]
}
