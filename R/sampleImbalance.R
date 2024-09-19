#' Sampling for Survival Class Imbalances
#'
#' Implements up- or down-sampling for analyses with binary class imbalances.
#'
#' @param data A data frame. For example, an individual
#' "analysis" fold ([rsample::analysis()]) from an [rsample::vfold_cv()] call.
#' @param var Character. A column name of `data` containing class labels
#' for up-/down-sampling. Must contain a binary variable.
#' `tidyselect` variable selection is also supported. See Examples.
#' @param method Character (matched). One of "down" or "up". Down-sampling
#' *decreases* the `n` per group to that of the smaller group size, whereas
#' up-sampling *increases* (sampling with replacement) the `n` per group to
#' that of the larger group size. The default is "down".
#' @return A sub- or re-sampled data frame that is smaller or larger than
#' `data` with equal class size.
#' @examples
#' xtab <- globalr::crossTabulate
#'
#' # original class distribution
#' xtab(mtcars, vs)
#'
#' # down-sampling `vs`
#' si <- sampleImbalance(mtcars, "vs")    # 'down'
#' xtab(si, vs)
#'
#' # up-sampling `vs` & passed variable
#' var <- "vs"
#' si <- sampleImbalance(mtcars, var, "up")
#' xtab(si, vs)
#'
#' # also supports unquoted strings
#' si  <- sampleImbalance(mtcars, vs)
#' xtab(si, vs)
#' @author Stu Field
#' @export
sampleImbalance <- function(data, var, method = c("down", "up")) {
  method   <- match.arg(method)
  .replace <- switch(method, down = FALSE, up = TRUE)
  spl_vec  <- tryCatch(data[[var]], error = function(e) NULL)
  # if NULL, lazyeval 'var' and eval promise in 'data' to pull variable
  spl_vec  <- spl_vec %||% eval(substitute(var), envir = data)
  # spl_vec <- dplyr::pull(data, !!enquo(var)) # nolint: commented_code_linter.
  stopifnot(length(unique(spl_vec)) == 2L)      # check binary
  .n <- switch(method, down = min(table(spl_vec)),
                         up = max(table(spl_vec)))
  group_df <- split(data, spl_vec)   # group split by `var`
  group_df |>                        # sample each group-split independently
    lapply(dplyr::sample_n, size = .n, replace = .replace) |>
    rbindIntersect() |>      # keeps rownames
    select(-data)            # rm `data` column added by `rbindIntersect()`
}
