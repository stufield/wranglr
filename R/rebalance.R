#' Sampling for Class Imbalances
#'
#' Implements up- or down-sampling for analyses
#'   with binary class imbalances.
#'
#' @param data A data frame or tibble. For example, an individual
#'   "analysis" fold ([rsample::analysis()]) from an [rsample::vfold_cv()] call.
#' @param var `character(1)`. A column name of `data` containing class labels
#'   for up-/down-sampling. Assumes a binary variable.
#'   `tidyselect` variable selection is also supported (see examples).
#' @param method `character(1)`. One of "down" or "up" (matched).
#'   Down-sampling *decreases* size of the major class,
#'   whereas up-sampling *increases* (i.e. sampling with
#'   replacement) the size per group of the minor class.
#'
#' @return A sub- or re-sampled data frame that is either smaller or
#'   larger than `data` with equal class balance.
#' @author Stu Field
#'
#' @examples
#' xtab <- helpr::cross_tab
#'
#' # original class distribution
#' xtab(mtcars, vs)
#'
#' # down-sampling `vs`
#' si <- rebalance(mtcars, "vs")    # 'down'
#' xtab(si, vs)
#'
#' # up-sampling `vs` & passed variable
#' var <- "vs"
#' si <- rebalance(mtcars, var, "up")
#' xtab(si, vs)
#'
#' # also supports unquoted strings
#' si  <- rebalance(mtcars, vs)
#' xtab(si, vs)
#'
#' @export
rebalance <- function(data, var, method = c("down", "up")) {
  method   <- match.arg(method)
  .replace <- switch(method, down = FALSE, up = TRUE)
  spl_vec  <- tryCatch(data[[var]], error = function(e) NULL)
  # if NULL, lazyeval 'var' and eval promise in 'data' to pull variable
  spl_vec  <- spl_vec %||% eval(substitute(var), envir = data)
  # spl_vec <- dplyr::pull(data, !!enquo(var)) # nolint: commented_code_linter.
  stopifnot(length(unique(spl_vec)) == 2L)      # check binary

  .n <- switch(method, down = min(table(spl_vec)), up = max(table(spl_vec)))

  group_df <- split(data, spl_vec)   # group split by `var`

  group_df |>                # sample each group-split independently
    lapply(dplyr::sample_n, size = .n, replace = .replace) |>
    bind_intersect() |>      # keeps rownames
    select(-data)            # rm `data` column added by `bind_intersect()`
}
