#' Convert Wide & Long Mixed Data to Long Format
#'
#' Takes a data frame object which has time series clinical
#' measurements which are stratified across columns in wide format.
#' Thus the data exists in both "long" *and* "wide" format and contains
#' duplicated information.
#' Clinical variables are typically named in a specific format:
#' \verb{<clinical_variable><clin.sep><timepoint>}.
#' The purpose of [pivotLong()] is to restructure the data to standard
#' long format used in SomaLogic proteomic analyses.
#'
#' @param data A clinical data frame with *both* "wide" and "long"
#'   format containing duplicated clinical information.
#' @param sub.key Character. A quoted string of the column name containing
#'   a unique identifier corresponding to each individual.
#' @param tp.key Character. A quoted string identifying the variable (column)
#'   name that contains the time point index for the proteomic data. This
#'   variable is *hard-coded* in the form \verb{"IE<x>"} (`x` = time-point),
#'   and is over-written during the process.
#' @param clin.sep Character. A quoted string containing a
#'   unique separator which exists in a variable name between the
#'   clinical variable and the time point index.
#' @return A data frame object of the same class as `data` converted
#'   to long format. The returned object has one row per sample
#'   identifier, only one column for a given clinical covariate, and
#'   the same number of rows as the input `data`.
#' @author Stu Field, Jeremy Primus
#' @examples
#' widelong <- tibble::tibble(
#'   subject_id    = c("a", "a", "a", "b", "b", "b"),
#'   ie            = c("IE1", "IE2", "IE3", "IE1", "IE2", "IE3"),
#'   clin_var1_ie1 = c(5L, 5L, 5L, 20L, 20L, 20L),
#'   clin_var1_ie2 = c(10L, 10L, 10L, 25L, 25L, 25L),
#'   clin_var1_ie3 = c(15L, 15L, 15L, 30L, 30L, 30L),
#'   clin_var2_ie1 = c(1.5, 1.5, 1.5, 7.7, 7.7, 7.7),
#'   clin_var2_ie2 = c(9.9, 9.9, 9.9, 9.2, 9.2, 9.2),
#'   clin_var2_ie3 = c(8.8, 8.8, 8.8, 1.6, 1.6, 1.6),
#'   clin_var3_ie1 = c("N", "N", "N", "Y", "Y", "Y"),
#'   clin_var3_ie3 = c("Y", "Y", "Y", "N", "N", "N"),
#'   seq.15529.33  = seq(34567, 45678, length.out = 6)
#' )
#'
#' piv <- pivotLong(widelong)
#' piv
#'
#' # samples (rows) by id are preserved
#' all.equal(nrow(piv), nrow(widelong))
#' @importFrom tidyr pivot_longer
#' @importFrom SomaDataIO getAnalytes getMeta
#' @export
pivotLong <- function(data, sub.key = "subject_id", tp.key = "ie",
                      clin.sep = "_ie") {

  clin   <- data[, getMeta(data)]
  apt_df <- data[, c(sub.key, tp.key, getAnalytes(data))]

  # select unique samples among variables which contain clin.sep
  # unique set of measurements
  unique_df <- distinctAt(clin, contains(clin.sep))
  clin_vars <- .strip_pat(names(unique_df), clin.sep) |> unique()

  # iterate over clinical vars for each time point
  # and collect values for each variables by time
  clin_tbls <- lapply(clin_vars, function(.x) {
    # get clin var names at each time point
    nms <- grep(.x, names(unique_df), value = TRUE)
    # strip the clinical pattern; leaving the time point
    time <- sub(paste0(".*", clin.sep), "", nms)
    # pivot table long-wise for each clinical variable/timepoint
    ret <- pivot_longer(unique_df, contains(.x), values_to = .x)
    # over-write the existing 'tp.key' column to correspond to 'time-point' value
    ret[[tp.key]] <- paste0("IE", rep(time, nrow(unique_df)))
    # strip out variables not interested in (other variable/timepoint combos)
    dplyr::select(ret, -contains(clin.sep), -name)
  })
  by_arg <- c(sub.key, tp.key)
  names(by_arg) <- by_arg
  # merge back in the list of tibbles onto each other
  clin_long <- Reduce(function(...) full_join(..., by = by_arg), clin_tbls)
  stopifnot(nrow(clin_long) == nrow(apt_df))   # sanity check; should not change
  ret <- left_join(apt_df, clin_long, by = by_arg) # merge back RFU data
  dplyr::select(ret, all_of(sub.key), all_of(tp.key), getMeta(ret), everything())
}

# strip the clinical pattern from string
.strip_pat <- function(x, pattern) {
  m <- grep(pattern, x, value = TRUE)
  sub(paste0(pattern, ".*"), "", m)
}
