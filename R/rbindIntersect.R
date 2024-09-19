#' Vertically Combine Data Frames by Intersect
#'
#' For [rbindIntersect()]: [rbind()] is used to vertically **combine** data
#' frames based on the _intersect_ of their column names.
#' This creates _fewer_ columns than the original data, or at best the same
#' number of columns. The resulting data frame has the dimensions:
#' \itemize{
#'   \item rows: `nrow(df1) + nrow(df2) + ... + nrow(df_n)`
#'   \item cols: `intersect(names(...))`
#' }
#'
#' Incidentally, the default behavior of [rbind()] reorders the columns
#'   correctly, but will only do so if their intersect matches.
#'
#' @param ... Data frames to combine. Can also be a _list_ of data frames
#'   to combine, typically `soma_adat` objects.
#' @return A single data frame with the total number of rows =
#'   `sum(sapply(..., nrow))`.
#' @note For [rbindIntersect()], columns are combined on their _intersect_ only.
#' @author Stu Field
#' @seealso [Reduce()], [rbind()], [intersect()]
#' @examples
#' # For `rbindIntersect()`
#' spl <- split(mtcars, mtcars$cyl) |> unname()
#' foo <- mapply(spl, -c(11, 10, 9), FUN = function(x, y) x[, y], SIMPLIFY = FALSE)
#' sapply(spl, names)
#' sapply(spl, ncol)
#'
#' # Pass a list
#' rbindIntersect(spl)
#'
#' # Can pass either list or via '...'
#' identical(rbindIntersect(spl), rbindIntersect(spl[[1L]], spl[[2L]], spl[[3L]]))
#'
#' # Passing a _named_ list adds `data` column with those names
#' names(spl) <- letters[1:3L]
#' rbindIntersect(spl)
#'
#' @export
rbindIntersect <- function(...) {
  dlist  <- .checkdots(...)
  common <- Reduce(intersect, lapply(dlist, names))
  lapply(dlist, `[`, common) |> rbindUnion()
}

#' Vertically Combine Data Frames by Union
#'
#' @rdname rbindIntersect
#' @description For [rbindUnion()]: [rbind()] is used to vertically **merge**
#'   data frames based on the _union_ of their column names. This creates
#'   columns of `NAs` for the rows of a data frame with non-overlapping
#'   column names. The resulting data frame has the dimensions:
#' \itemize{
#'   \item rows: `nrow(df1) + nrow(df2) + ... + nrow(df_n)`
#'   \item cols: `union(names(...))`
#' }
#' @note For [rbindUnion()], the ordering of the rows correspond to the order
#'   they are supplied.
#' @seealso [union()]
#' @examples
#' # For `rbindUnion()`
#' rbindUnion(spl)
#' rbindUnion(spl[[1L]], spl[[2L]])
#' rbindUnion(spl[[1L]], spl[[2L]], spl[[3L]])
#' @importFrom SomaDataIO col2rn getMeta rn2col
#' @export
rbindUnion <- function(...) {
  x <- .checkdots(...)
  lapply(x, rn2col) |> .bind_and_clean()
}

#' @noRd
#' @param x A list of data frames.
.bind_and_clean <- function(x) {
  out <- bind_rows(x, .id = "data") |> col2rn()
  select(out, getMeta(out), everything())
}

#' @noRd
.checkdots <- function(...) {
  dots <- list(...)
  # if nested list, return the list as is
  if ( length(dots) == 1L && typeof(dots[[1L]]) == "list" ) {
    dots <- unlist(dots, recursive = FALSE)
  }
  df_lgl <- vapply(dots, is.data.frame, NA)
  if ( !all(df_lgl) ) {
    stop("All `...` must be data frames!", call. = FALSE)
  }
  # add names if absent
  if ( is.null(names(dots)) ) {
    names(dots) <- sprintf("data_%02i", seq_along(dots))
  }
  dots
}
