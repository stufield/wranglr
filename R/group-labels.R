#' Return Grouping Labels
#'
#' Returns a `tibble` of the grouping labels (usually class labels)
#' corresponding a `grouped_df` class object.
#'
#' @param x A data frame or extension (like a `tibble` or `grouped tibble`).
#' @author Stu Field
#' @seealso [group_vars()], [group_data()]
#' @examples
#' df <- dplyr::group_by(mtcars, cyl)
#' class(df)
#' dplyr::group_data(df)
#' dplyr::group_vars(df)
#'
#' # 1 variable
#' group_labels(df)
#'
#' # 2 variables
#' df <- dplyr::group_by(mtcars, cyl, vs)
#' group_labels(df)
#' @export
group_labels <- function(x) {
  stopifnot("`x` must be a grouped object." = is.grouped_df(x))
  dplyr::group_data(x) |>
    dplyr::select(dplyr::group_vars(x))
}
