#' Create A Summary Table
#'
#' Create a summary table of stratified by various grouping
#'   structures and a pre-defined set of summary statistics:
#'   \itemize{
#'     \item `min`
#'     \item `max`
#'     \item `mean`   (arithmetic mean)
#'     \item `sd`     (standard deviation)
#'     \item `median` (50th percentile)
#'     \item `mad`    (median absolute deviation)
#'     \item `mode`   (most common value)
#'     \item `Q25`    (lower quartile)
#'     \item `Q75`    (upper quartile)
#'     \item `CV`     (coefficient of variance)
#'   }
#'
#' @param data A `data.frame` or `tibble` object containing data
#'   for summary.
#'
#' @param var `character(1)`. An unquoted string containing the
#'   column name to summarize.
#'
#' @param ... One or more unquoted column names containing grouping
#'   information. Passed to `dplyr::group_by()`. The first grouping
#'   column is coerced to `character` in the output to accommodate
#'   the summary "Total" row.
#'
#' @return A `tibble` object with rows (groups) and columns as the
#'   standard summary statistics.
#'
#' @author Stu Field
#' @seealso [mad()], [median()], [sd()], [min()], [max()], [IQR()]
#'
#' @examples
#' create_sumry_tbl(mtcars, mpg, cyl)
#'
#' create_sumry_tbl(mtcars, mpg, cyl, am)
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
  # Coerce the first grouping var so a "Total"
  #   row can rbind cleanly
  # Trade-off: first output column type is always character.
  data <- data |>
    mutate(across(all_of(shim), as.character))
  total <- ungroup(data) |>
    summarise(.calc_stats(!!ensym(var)))
  total[[shim]] <- "Total"
  data |>
    group_by(...) |>
    summarise(.calc_stats(!!ensym(var)), .groups = "drop") |>
    bind_rows(total)
}


# internal
.calc_stats <- function(x) {
  L   <- length(x)
  nas <- sum(is.na(x))
  n   <- L - nas
  x   <- x[!is.na(x)]
  # All-NA guard: return an all-NA row directly. This avoids
  #   `quantile()` erroring on `NA_real_` and prevents `min()` /
  #   `max()` warnings on empty vectors.
  if ( n == 0L ) {  # catch for when ALL `x` are NA
    y <- NA_real_
    tibble(
      total_n = L, NAs = nas, n = 0L, min = y, max = y, mean = y,
      sd = y, median = y, mad = y, mode = y, Q25 = y, Q75 = y, CV = y
    )
  } else {
    tibble(
      total_n = L,
      NAs   = nas,
      n     = n,
      min   = min(x),
      max   = max(x),
      mean  = mean(x),
      sd    = sd(x),
      median = stats::median(x),
      mad   = stats::mad(x, constant = 1.0),
      mode  = .calc_mode(x),
      Q25   = stats::quantile(x, 0.25, names = FALSE),
      Q75   = stats::quantile(x, 0.75, names = FALSE),
      CV    = sd / mean
    )
  }
}


#' @importFrom helpr %||-%
#' @noRd
.calc_mode <- function(x) {
  x <- x[!is.na(x)]
  as.numeric(names(which.max(table(x)))) %||-% NA_real_
}
