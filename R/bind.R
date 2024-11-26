#' Vertically Combine Data Frames by Intersect
#'
#' For [bind_intersect()]: [rbind()] is used to vertically **combine** data
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
#' @name bind
#' @param ... Data frames to combine. Can also be a _list_ of data frames
#'   to combine.
#' @return A single data frame with the total number of rows =
#'   `sum(sapply(..., nrow))`.
#' @note For [bind_intersect()], columns are combined on
#'   their *intersect* only.
#' @author Stu Field
#' @seealso [Reduce()], [rbind()], [intersect()]
#' @examples
#' # For `bind_intersect()`
#' spl <- split(mtcars, mtcars$cyl) |> unname()
#' foo <- mapply(spl, -c(11, 10, 9), FUN = function(x, y) x[, y], SIMPLIFY = FALSE)
#' sapply(spl, names)
#' sapply(spl, ncol)
#'
#' # Pass a list
#' bind_intersect(spl)
#'
#' # Can pass either list or via '...'
#' identical(bind_intersect(spl), bind_intersect(spl[[1L]], spl[[2L]], spl[[3L]]))
#'
#' # Passing a *named* list adds `data` column with those names
#' names(spl) <- letters[1:3L]
#' bind_intersect(spl)
#'
#' @export
bind_intersect <- function(...) {
  dlist  <- .checkdots(...)
  common <- Reduce(intersect, lapply(dlist, names))
  lapply(dlist, `[`, common) |>
    bind_union()
}

#' Vertically Combine Data Frames by Union
#'
#' @rdname bind
#' @description For [bind_union()]: [rbind()] is used to vertically **merge**
#'   data frames based on the _union_ of their column names. This creates
#'   columns of `NAs` for the rows of a data frame with non-overlapping
#'   column names. The resulting data frame has the dimensions:
#' \itemize{
#'   \item rows: `nrow(df1) + nrow(df2) + ... + nrow(df_n)`
#'   \item cols: `union(names(...))`
#' }
#' @note For [bind_union()], the ordering of the rows correspond
#'   to the order they are supplied.
#' @seealso [union()]
#' @examples
#' # For `bind_union()`
#' bind_union(spl)
#' bind_union(spl[[1L]], spl[[2L]])
#' bind_union(spl[[1L]], spl[[2L]], spl[[3L]])
#' @importFrom helpr col2rn has_rn rn2col
#' @export
bind_union <- function(...) {
  .checkdots(...) |>
    .bind_and_clean()
}

#' @noRd
#' @param x A list of data frames.
.bind_and_clean <- function(x) {
  out <- bind_rows(x, .id = "data")
  if ( ".bindr" %in% names(out) ) {
    out <- col2rn(out, ".bindr")  # rm rn col is present
  }
  select(out, get_meta(out), everything())
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
  # maintain rn if present
  rn_lgl <- vapply(dots, has_rn, NA)
  if ( any(rn_lgl) ) {
    dots <- lapply(dots, rn2col, name = ".bindr")
  }
  dots
}
