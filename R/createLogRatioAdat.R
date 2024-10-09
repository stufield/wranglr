#' Calculate log-Ratios by TimePoint
#'
#' Create a data frame (`soma_adat`) with entries corresponding
#' to the log2-ratios by individual `subject.id` for each of the
#' time points provided in the `time` argument.
#'
#' If `rm.baseline = FALSE`, the baseline samples are retained for
#' consistency of the meta data, but values will be zero. This
#' particularly makes sense for plotting where there baseline
#' "zeros" might be desired if `TRUE`, the number of data frame
#' rows will correspond to `nrow(adat) - n.subjects`, with the baseline
#' sample being removed for each subject.
#'
#' @param adat A `soma_adat` or data frame object containing feature
#'   data, subject IDs (PIDs), and time point information.
#' @param subject.field An unquoted string of the subject ID field, containing
#'   identifiers for the patients/subjects with multiple measurements.
#' @param time.field An unquoted string of the time point field. Must have at
#'   least 2 levels. Particular attention should be made to ensure that the
#'   baseline sample name can be sorted in a meaningful way, i.e.
#'   `sort(unique(adat$time.field))` generates the order you expect.
#' @param rm.baseline Logical. Should the baseline samples be removed in
#'   the returned data frame?
#' @return Returns a data frame of the log2-ratios vs. baseline at each time
#'   point for each individual in the data set. The result is sorted by subject
#'   and then by time.
#' @author Stu Field
#' @seealso [log2()]
#' @examples
#' sim_test_data$time <- rep(1:4, 25)        # random time points
#' sim_test_data$pid  <- rep(1:25, each = 4)  # assign random subjects
#' a1 <- createLogRatioAdat(sim_test_data, pid, time)    # keep all
#' dim(a1)
#' a2 <- createLogRatioAdat(sim_test_data, pid, time, TRUE)  # remove baseline
#' dim(a2)
#' @importFrom SomaDataIO getAnalytes rm_rn col2rn
#' @export
createLogRatioAdat <- function(adat, subject.field, time.field,
                               rm.baseline = FALSE) {
  apts  <- getAnalytes(adat)
  .time <- substitute(time.field)
  .id   <- substitute(subject.field)
  subject_vec <- dplyr::pull(adat, !!.id)
  logr  <- function(.x) log2(.x / .x[1L])
  adat2 <- arrange(adat, !!.id, !!.time) |> split(subject_vec)
  ret <- lapply(adat2, function(.x) {
    .x[, apts] <- apply(.x[, apts], 2, logr)
    .x
    }) |>
    do.call(what = rbind) |>
    mutate(UniqueId = paste0(SlideId, "_", Subarray)) |>
    rm_rn() |>        # nuke rownames to be remade below
    col2rn("UniqueId")
  if ( rm.baseline ) {
    pids <- unique(subject_vec) |> sort()
    ret  <- dplyr::filter(ret, !!.time != pids[1L]) |> refactor_data()
    signal_done("Removing", value(nrow(adat) - nrow(ret)), "baseline samples.")
  }
  ret
}
