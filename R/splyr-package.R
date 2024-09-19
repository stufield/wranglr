
#' @keywords internal package
"_PACKAGE"

#' @import dplyr
NULL

.dummy <- function() { }   # nolint: brace_linter.

.onLoad <- function(...) {
  pkgenv <- environment(.dummy)
  # Re-export data for internal/testing
  utils::data("sim_test_data", package = "splyr", envir = pkgenv)
  invisible()
}
