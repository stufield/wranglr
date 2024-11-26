#' Create A Summary Table
#'
#' Create a summary table of all the feature data based on a
#'   grouping variable of meta data. Defaults summary statistics include:
#'   \itemize{
#'     \item `min`
#'     \item `median`
#'     \item `mean`
#'     \item `sd`  (standard deviation)
#'     \item `mad` (median absolute deviation)
#'     \item `max`
#'   }
#'
#' @param data A `data.frame` or `tibble` object containing data for summary.
#' @param group_var `character(1)`. An unquoted (or quoted) string
#'   containing the indices to group the statistics,
#'   e.g. `Group`. If missing, ungrouped statistics are returned.
#' @param .funs `character(n)`. String(s) of the functions used to
#'   summarize the data. Each function *must* take a vector of data
#'   as input and return a summary scalar, e.g. [mean()].
#'
#' @return A `tibble` object with rows (features) and columns as the
#'   summary statistics listed in `.funs`.
#' @author Stu Field
#'
#' @seealso [mad()], [median()], [sd()], [min()], [max()]
#'
#' @examples
#' create_summ_tbl(sim_adat)
#' create_summ_tbl(sim_adat, gender)
#'
#' # Arbitrary 3 groupings
#' sim_adat$group <- sample(1:3, nrow(sim_adat), replace = TRUE)
#' create_summ_tbl(sim_adat, "group")
#' @importFrom stats mad median
#' @importFrom helpr set_Names
#' @importFrom tibble as_tibble
#' @importFrom tidyr nest unnest pivot_wider
#' @export
create_summ_tbl <- function(data, group_var, .funs = c("min",
                                                       "median",
                                                       "mean",
                                                       "sd",
                                                       "mad",
                                                       "max")) {

  # If no grouping variable, create "dummy" vector for nesting
  if ( missing(group_var) ) {
    data$group_var <- ""
    sep <- ""
  } else {
    gr_vec <- tryCatch(data[[group_var]], error = function(e) NULL)
    data$group_var <- gr_vec %||% eval(substitute(group_var), envir = data)
    sep <- "_"
  }

  # Calculate '.funs' statistics for 1 column vector of data
  summ_vec <- function(vec) {
    lapply(set_Names(.funs), function(.x) get(.x)(vec)) |> as_tibble()
  }
  # Apply summary statistics across all features
  summ_df <- function(df) bind_rows(lapply(df, summ_vec))

  # Create nested-tibble of data sets by group
  data_nest <- data |>
    select(group_var, get_analytes(data)) |>
    arrange(group_var) |>   # for below; columns ordered by group levels
    group_by(group_var) |>  # split out by group
    nest()

  # Do the calculations on list columns
  data_nest$summary <- lapply(data_nest[["data"]], summ_df)
  data_nest$Feature <- lapply(data_nest[["data"]], names)

  # Re-org, un-nest, spread the summaries; and return
  select(data_nest, group_var, summary, Feature) |>
    unnest(cols = c(Feature, summary)) |>
    pivot_wider(names_from = group_var,
                values_from = all_of(.funs), names_sep = sep)
}
