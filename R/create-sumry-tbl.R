#' Create A Summary Table
#'
#' Create a summary table of stratified by various grouping
#'   structures and a pre-defined set of summary statistics:
#'   \itemize{
#'     \item `min`
#'     \item `max`
#'     \item `mean`
#'     \item `sd`  (standard deviation)
#'     \item `median`
#'     \item `mad` (median absolute deviation)
#'     \item `mode`
#'     \item `IQR`
#'     \item `CV`
#'   }
#'
#' @param data A `data.frame` or `tibble` object containing data for summary.
#' @param var `character(1)`. An unquoted string containing the
#'   column name to summarize.
#' @param ... One or more unquoted column names containng grouping
#'   information. Passed to `dplyr::group_by()`.
#'
#' @return A `tibble` object with rows (groups) and columns as the
#'   standard summary statistics.
#' @author Stu Field
#'
#' @seealso [mad()], [median()], [sd()], [min()], [max()], [IQR()]
#'
#' @examples
#' mt <- mutate(mtcars, cyl = as.factor(cyl), am = as.factor(am))
#'
#' create_sumry_tbl(mt, mpg, cyl)
#'
#' create_sumry_tbl(mt, mpg, cyl, am)
#' @importFrom tibble tibble
#' @export
create_sumry_tbl <- function(data, var, ...) {
  dots <- function(...) nargs()
  if ( dots(...) == 0 ) {
    stop("You must provide a grouping column in `data`.",
         call. = FALSE)
  } else {
    shim <- toString(substitute(...))[1L]
  }
  total <- ungroup(data) |>
    summarise(n = n(), .calc_stats(!!ensym(var)))
  total[[shim]] <- "Total"
  data |>
    group_by(...) |>
    summarise(n = n(), .calc_stats(!!ensym(var)), .groups = "drop") |>
    bind_rows(total)
}


# internal
.calc_stats <- function(x) {
  nas <- sum(is.na(x))
  x <- x[!is.na(x)]
  tibble(
    NAs   = nas,
    min   = min(x),
    max   = max(x),
    mean  = mean(x),
    sd    = sd(x),
    median = stats::median(x),
    mad   = stats::mad(x, constant = 1),
    mode  = .calc_mode(x),
    IQR   = stats::IQR(x),
    CV    = sd / mean
  )
}

.calc_mode <- function(x) {
  x <- x[!is.na(x)]
  as.numeric(names(which.max(table(x))))
}
