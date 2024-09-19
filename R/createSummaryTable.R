#' Create A Summary Table
#'
#' Create a summary table of all the feature/SOMAmer data based on a
#' grouping variable of meta data. Defaults summary statistics include:
#'   * `min`
#'   * `median`
#'   * `mean`
#'   * `sd`  (standard deviation)
#'   * `mad` (median absolute deviation)
#'   * `max`
#'
#' @param adat A `soma_adat` or `data.frame` object containing RFU data.
#' @param group.var An unquoted (or quoted) string containing the indices
#'   to group the statistics, e.g. `SampleGroup`. If missing, ungrouped
#'   statistics are returned.
#' @param .funs Character. Strings of the functions used to
#'   summarize the data. Each function _must_ take a vector of data as input and
#'   return a scalar summary.
#' @return A `tibble` object with `AptNames` for rows and columns as the
#'   summary statistics listed in `.funs`.
#' @author Stu Field
#' @seealso [mad()], [median()], [sd()], [min()], [max()]
#' @examples
#' createSummaryTable(sim_test_data)
#' createSummaryTable(sim_test_data, gender)
#'
#' # Arbitrary 3 groupings
#' sim_test_data$Group <- sample(1:3, nrow(sim_test_data), replace = TRUE)
#' createSummaryTable(sim_test_data, "Group")
#' @importFrom stats mad median setNames
#' @importFrom tibble as_tibble
#' @importFrom tidyr nest unnest pivot_wider
#' @importFrom SomaDataIO getAnalytes
#' @export
createSummaryTable <- function(adat, group.var, .funs = c("min",
                                                          "median",
                                                          "mean",
                                                          "sd",
                                                          "mad",
                                                          "max")) {

  # If no grouping variable, create "dummy" vector for nesting
  if ( missing(group.var) ) {
    adat$group_var <- ""
    sep <- ""
  } else {
    gr_vec <- tryCatch(adat[[group.var]], error = function(e) NULL)
    adat$group_var <- gr_vec %||% eval(substitute(group.var), envir = adat)
    sep <- "_"
  }

  # Calculate '.funs' statistics for 1 column vector of data
  summ_vec <- function(vec) {
    lapply(setNames(.funs, .funs), function(.x) get(.x)(vec)) |> as_tibble()
  }
  # Apply summary statistics across all Aptamers
  summ_df <- function(df) bind_rows(lapply(df, summ_vec))

  # Create nested-tibble of data sets by group
  adat_nest <- adat |>
    select(group_var, getAnalytes(adat)) |>
    arrange(group_var) |>   # for below; columns ordered by group levels
    group_by(group_var) |>  # split out by group
    nest()

  # Do the calculations on list columns
  adat_nest$summary <- lapply(adat_nest[["data"]], summ_df)
  adat_nest$AptName <- lapply(adat_nest[["data"]], names)

  # Re-org, un-nest, spread the summaries; and return
  select(adat_nest, group_var, summary, AptName) |>
    unnest(cols = c(AptName, summary)) |>
    pivot_wider(names_from = group_var,
                values_from = all_of(.funs), names_sep = sep)
}
