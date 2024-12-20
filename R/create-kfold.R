#' Create k-Fold Partitioning
#'
#' Adaptation of the `rsample::vfold_cv()` function and its utilities.
#'   Modified for simplicity and to accommodate two-level stratification.
#'   Up to 2 levels of stratification can be specified through
#'   the `breaks` parameter:
#'  \itemize{
#'    \item{No stratification: `breaks = NULL`}
#'    \item{One level stratification: `breaks` is a `list(1)`, where
#'      the name of the list element specifies the stratification
#'      variable and the value of the element specifies the
#'      stratification.}
#'    \item{Two level stratification: `breaks` is a `list(2)`, where
#'      the list names specify the stratification variables and the
#'      corresponding values specify the stratification.}
#'  }
#'
#' For stratification variables that are factor, character, or discrete with
#'   5 or fewer unique levels, the stratification structure should be set
#'   to `NULL`. For example, if `status` is a binary variable,
#'   `breaks = list(status = NULL)`.
#'
#' If the stratification variable has more than 5 unique levels,
#'   the stratification structure can be specified as either the
#'   number of quantile-based stratification bins or as a numeric vector
#'   providing the bin boundaries (must fully span the range of the
#'   stratification variable). For example, for a continuous variable
#'   in \verb{[0,1]}, `breaks = list(x = 4)` indicates stratification
#'   into 4 bins, the boundaries of which are determined internally using
#'   ```{r eval = FALSE}
#'   quantile(x, probs = seq(0.0, 1.0, length.out = 5))
#'   ```
#'   and `breaks = list(x = c(0.0, 0.25, 0.75, 1.0))` specifies a 3 bin
#'   structure: \verb{[0, 0.25]}, \verb{(0.25, 0.75]}, and \verb{(0.75, 1.0]}.
#'    **Note:** the lowest bin is *always* inclusive.
#'
#' @param data A `data.frame` containing the data to be subset.
#' @param k `integer(1)`. The number of partitions (folds) to create.
#' @param repeats `integer(1)`The number of times to repeat the *k*-fold
#'   partitioning.
#' @param breaks Stratification control. Either `NULL` or a *named* list.
#'   If `NULL`, no stratification is performed. See `Details`.
#' @param ... Parameters to be passed to stratification step. Currently limited
#'   to `depth`. The number of stratification bins are based on
#'   `min(5, floor(n / depth))`, where `n = length(x)`.
#'
#' @return A `x_split` object. Element `data` contains the original data.
#'   Element `splits` is a tibble where each row of corresponds to an
#'   individual split. The `split` column contains lists named either
#'   "analysis" and "assessment" containing the indices of `data` to be
#'   used for each fold and category.
#'   Columns `Fold` and `Repeat` provide fold and repeat indices for
#'   each corresponding split.
#'
#' @examples
#' # no stratification
#' no_strat <- create_kfold(mtcars, k = 4L, repeats = 2L)
#'
#' # stratification on 1 discrete variable
#' sample_one <- create_kfold(mtcars, k = 4L, repeats = 2L,
#'                            breaks = list(vs = NULL))
#'
#' # stratification on 2 variables; 1 continuous + 1 discrete
#' sample_two <- create_kfold(mtcars, k = 4L, repeats = 2L,
#'                            breaks = list(gear = 4L, vs = NULL))
#'
#' @importFrom helpr is_int add_class dots_list2
#' @export
create_kfold <- function(data, k = 10L, repeats = 1L, breaks = NULL, ...) {

  stopifnot(
    "`data` must be a `data.frame`." = !missing(data) && is.data.frame(data),
    "`breaks` must be `NULL` or a list of length 1 or 2." =
      is.null(breaks) || (is.list(breaks) && length(breaks) %in% 1:2L)
  )

  check_int(k)
  check_int(repeats)

  # user can currently only pass `depth` through ...
  # could expand to allow for `n_unique`
  # both are used in stratification
  args    <- dots_list2(...)
  depth   <- args$depth %||% 20L
  repeats <- as.integer(round(repeats, 0L))

  splits <- replicate(repeats,
    .vfold_splits(data = data, k = k, breaks = breaks, depth = depth),
    simplify = FALSE
  ) |>
    bind_rows(.id = ".repeat") |>
    relocate(-.repeat)

  if ( repeats == 1L ) {
    splits$.repeat <- NA_integer_
  } else {
    splits$.repeat <- as.integer(splits$.repeat)
  }

  return_obj <- list(data = data, splits = splits)
  .call <- match.call()

  structure(
    return_obj,
    class = c("x_split", "list"),
    k = k,
    repeats = repeats,
    breaks = breaks,
    call = .call
  )
}

#' Adaptation of `rsample::vfold_splits()` function to return only indices
#'
#' Function processes stratification variable, calls stratification function,
#'   and post-processes the stratification result into a tibble.
#'
#' @param data A `data.frame` class object. The data to be subset.
#' @param k `integer(1)`. The number of partitions to create.
#' @param breaks See `create_kfolds()`.
#' @param depth `integer(1)`. Used to determine the best number of bins
#'   to be used. The number of bins are based on `min(5, floor(n / depth))`
#'   where `n = length(x)`. If `x` is numeric, there must be at least 40
#'   rows in the data set (when `depth = 20L`) to conduct stratified sampling.
#'
#' @return See description section of `create_kfolds()`.
#'
#' @importFrom tibble tibble
#' @noRd
.vfold_splits <- function(data, k = 10L, breaks = NULL, depth = 20L) {

  strata <- names(breaks) %||% NULL

  if ( is.list(breaks) ) {
    if ( is.null(strata) ) {
      stop("`breaks` must be a *named* list.", call. = FALSE)
    }
    if ( length(breaks) == 1L ) {
      breaks <- unlist(breaks)
    }
  }

  idx <- seq_len(nrow(data))

  if ( !is.null(strata) ) {
    # `drop = TRUE` ensures proper .get_indices() dispatch below
    x <- tryCatch(data.frame(data)[, strata, drop = TRUE],
                  error = function(e) {
                  stop("Unable to retrieve stratification variable(s) ",
                       "from `data`.\n\t", e$message, call. = FALSE)
                 })
    indices <- .get_indices(x, breaks = breaks, k = k, idx = idx, depth = depth)
  } else {
    # no strat
    folds  <- sample(rep(seq_len(k), length.out = length(idx)))
    indices <- unname(split(idx, folds))
  }

  # indices for the *assessment* group
  indices <- lapply(indices, function(.ind) {
               list(analysis   = setdiff(idx, .ind),
                    assessment = sort(unique(.ind)))
    })

  tibble(split = indices, fold = seq_len(k))
}


#' Select the stratification function
#'
#' @param k `integer(1)`. See `create_kfold()`
#' @param ... For extensibility to downstream methods.
#'
#' @return A list, each element providing the indices of the assessment data for
#'   a single fold.
#'
#' @importFrom helpr has_length is_int_vec value
#' @noRd
.get_indices <- function(x, k, ...) UseMethod(".get_indices")


#' @noRd
.get_indices.default <- function(x, k, ...) {
  stop("Unable to dispatch on `x` with unexpected class: ", value(class(x)),
         call. = FALSE)
}


#' S3 numeric method: 1 stratification variable
#'
#' @param x A vector of numeric values to stratify.
#' @param breaks Passed to `.create_strata()`
#' @param k `integer(1)`. See `create_kfold()`
#' @param idx `integer(n)`. The indices of the data to be stratified.
#' @param depth Passed to `.create_strata()`
#'
#' @importFrom helpr is_int_vec
#' @noRd
.get_indices.numeric <- function(x, k, idx, breaks, ...) {

  check_int(k)

  stopifnot(
    "`idx` must be an integer vector with dimension matching `x`." =
      is_int_vec(idx) && is.vector(idx) && length(idx) == length(x) && all(idx > 0)
  )

  if ( is.matrix(x) ) {
    stop("`x` must be numeric, not a ", value("matrix"), call. = FALSE)
  }
  if ( is.list(breaks) ) {
    stop("`breaks` must be a vector of cutoffs, not a ", value("list"),
         call. = FALSE)
  }

  stratas <- .create_strata(x = x, breaks = breaks, ...)
  # this produces a list of length(breaks)
  stratas <- unname(split(idx, stratas))
  stratas <- lapply(stratas, function(.x) {
               list(idx   = .x,
               folds = sample(rep(seq_len(k), length.out = length(.x))))
              })
  stratas <- bind_rows(stratas)
  unname(split(stratas$idx, stratas$folds))
}

#' @noRd
.get_indices.character <- function(x, k, idx, ...) {

  check_int(k)

  stopifnot(
    "`idx` must be an integer vector with dimension matching `x`." =
      is_int_vec(idx) && is.vector(idx) && length(idx) == length(x) && all(idx > 0)
  )

  stratas <- strat_and_impute(factor(x))
  stratas <- unname(split(idx, stratas))
  stratas <- lapply(stratas, function(.x) {
                    list(idx   = .x,
                         folds = sample(rep(seq_len(k), length.out = length(.x))))
                    })
  stratas <- bind_rows(stratas)
  unname(split(stratas$idx, stratas$folds))
}

#' @noRd
.get_indices.factor <- .get_indices.character


#' S3 data frame method: 2 stratification variables
#'
#' @param x A `data.frame` must have exactly *2* columns.
#' @param breaks A `list(2)`. Each element an integer or numeric vector.
#' @param k `integer(1)`. See `create_kfold()`
#' @param depth `integer(1)`. Used to determine the optimal number
#'   of percentiles to be used. The number of bins are based on
#'   `min(5, floor(n / depth))` where `n = length(x)`.
#'   If `x` is numeric, there must be at least 40 rows in
#'   the data set (when `depth = 20`) to conduct stratified sampling.
#'
#' @noRd
.get_indices.data.frame <- function(x, k, breaks, depth, ...) {

  stopifnot(
    "`data.frame` must have exactly 2 columns." = ncol(x) == 2L,
    "`breaks` must be a list of length 2." = is.list(breaks) && length(breaks) == 2L
  )

  b1 <- breaks[[1L]]
  b2 <- breaks[[2L]]

  # stratify on first variable
  strat1 <- .create_strata(x[[1L]], breaks = b1, depth = depth)
  strat1 <- unname(split(seq_len(nrow(x)), strat1))

  # stratify each element of strat1 using second stratification variable
  # should now be a vector: numeric, factor, or character
  lapply(strat1, function(.i) {
    .get_indices(x[.i, 2L, drop = TRUE], k = k,
                 breaks = b2, idx = .i, depth = depth)
  }) |>
    c(f = c) |> # invert and concatenate within elements (folds)
    do.call(what = "Map")
}


#' Single stratification variable
#'
#' @param x A *numeric* vector to be used for stratification.
#'   Character and factor classes are handled outside this function
#'   in `.get_indices()` methods.
#' @param breaks `integer(1)` or `numeric(n)`.
#'   If integer, the number of bins desired to stratify a numeric
#'   stratification variable.
#'   If a numeric vector, the bin boundaries to be used for
#'   stratification of a numeric stratification variable.
#' @param n_unique `integer(1)`. The number of unique value threshold
#'   in the algorithm. Currently there is no access point to this input.
#'   A natural extension is to allow the user to specify this as an
#'   input to `create_kfold()`.
#' @param depth `integer(1)`. Used to determine the best number of bins that
#'   should be used. The number of bins is based on `min(5, floor(n / depth))`
#'   where `n = length(x)`. If `x` is numeric, there must be at least 40 rows in
#'   the data set (when `depth = 20`) to conduct stratified sampling.
#'
#' @return A factor vector indicating the stratum for each `x`.
#'
#' @importFrom helpr has_length value len_one
#' @importFrom stats na.omit quantile
#' @noRd
.create_strata <- function(x, breaks = 4L, n_unique = 5L, depth = 20L) {

  stopifnot(
    "`x` must be a numeric vector." = inherits(x, c("numeric", "integer")) &&
      has_length(x)
  )

  check_int(depth)
  check_int(n_unique)
  n_vals <- unique(na.omit(x))

  # Do this part first
  # stratification variable has few unique values
  # no binning necessary; early return
  if ( length(n_vals) <= n_unique ) {
    out <- strat_and_impute(factor(as.character(x)))
    return(out)
  }

  # now do the rest
  # first check breaks
  stopifnot(
    "`breaks` must be an integer or a numeric vector." = .breaks_check(breaks)
  )

  # breaks provides the number of bins
  if ( len_one(breaks) ) {
    # ensure that the stratification will lead to "reasonable"
    # numbers of cases per bin

    n <- length(x)

    if ( floor(n / breaks) < depth ) {
      warning("The number of observations in each quantile is ",
              "below the recommended threshold of ", depth, ".\n",
              "Stratification will be done with ", floor(n / depth),
              " breaks instead.\n",
              "To override this limit, provide `depth` as input.",
              call. = FALSE)
      breaks <- min(breaks, floor(n / depth))
      if ( breaks <= 2 ) {
        stop("Too little data to stratify.\n",
             "Please consider non-stratified resampling via `breaks = NULL`.",
             call. = FALSE)
      }
    }
    breaks <- quantile(x, probs = seq(0.0, 1.0, length.out = breaks + 1L),
                       na.rm = TRUE)
  }

  out <- cut(imputeNAs(x), breaks = unique(breaks), include.lowest = TRUE)

  if ( any(is.na(out)) ) {
    # cut returns NA for values outside the range of `breaks`.
    stop("Provided `breaks` does not span data.\n\t",
         "Range of stratification variable: ", value(range(x, na.rm = TRUE)),
         "\n\tProvided `breaks = ", value(breaks), "`.",
         call. = FALSE
    )
  }

  out
}


#' Test for object class "x_split"
#'
#' @rdname create_kfold
#'
#' @param x An `R` object to test.
#'
#' @return [is.x_split()]: Logical. `TRUE` if `x` inherits from
#'   class `x_split`.
#' @export
is.x_split <- function(x) {
  inherits(x, "x_split")
}


#' Retrieve the analysis data from a `x_split` object.
#'
#' [analysis()]: `r lifecycle::badge("questioning")`
#'
#' @rdname create_kfold
#'
#' @param object A `x_split` object.
#' @param i `integer(1)` or `NULL`. If an integer, the split corresponding
#'   to the analysis or assessment data to be retrieved, otherwise a list of
#'   *all* data splits.
#'
#' @return [analysis()]: A list of data frames corresponding to
#'   the analysis indices.
#' @examples
#' # retrieve analysis data for 2nd split
#' an_2 <- analysis(no_strat, 2L)
#'
#' # retrieve all splits
#' an_all <- analysis(no_strat)
#'
#' @importFrom helpr is_int
#' @export
analysis <- function(object, i = NULL) {
  stopifnot(
    "`object` must be a `x_split` object." = is.x_split(object),
    "`i` must be a positive integer." = is.null(i) || check_int(i),
    "`i` cannot exceed the number of splits." = is.null(i) ||
      (is_int(i) && i <= length(object$splits$split))
  )

  if ( is.null(i) ) {
    lapply(object$splits$split, function(.x) object$data[.x$analysis, ])
  } else {
    list(object$data[object$splits$split[[i]]$analysis, ])
  }
}


#' Retrieve the assessment data from a `x_split` object.
#'
#' [assessment()]: `r lifecycle::badge("questioning")`
#'
#' @rdname create_kfold
#'
#' @return [assessment()]: A list of data frames corresponding to
#'   the assessment indices.
#'
#' @examples
#' # retrieve assessment data for 2nd split
#' ass_2 <- assessment(no_strat, 2L)
#'
#' # retrieve all splits
#' ass_all <- assessment(no_strat)
#' @importFrom helpr is_int
#' @export
assessment <- function(object, i = NULL) {
  stopifnot(
    "`object` must be a `x_split` object." = is.x_split(object),
    "`i` must be a positive integer." = is.null(i) || check_int(i),
    "`i` cannot exceed the number of splits." = is.null(i) ||
      (is_int(i) && i <= length(object$splits$split))
  )

  if ( is.null(i) ) {
    lapply(object$splits$split, function(.x) object$data[.x$assessment, ])
  } else {
    list(object$data[object$splits$split[[i]]$assessment, ])
  }
}


#' S3 method for `x_split` object
#'
#' @noRd
#' @importFrom helpr signal_rule value add_color
#' @export
print.x_split <- function(x, ...) {
  signal_rule("A `x_split` object", lty = "double", line_col = "magenta")
  call <- attr(x, "call")
  key <- c("k", "repeats", "stratified", "orig data") |> pad(12)
  value <- c(
    value(attr(x, "k")),
    value(attr(x, "repeats")),
    value(deparse(attr(x, "breaks"))), # can be `NULL`; deparse()
    value(call[["data"]])
  )

  liter(key, value, function(.x, .y) {
    writeLines(paste(add_color("\u2022", "red"), .x, .y))
  })

  signal_rule("split info", line_col = "cyan")
  print(x$splits)
  signal_rule("", lty = "double", line_col = "green")
  invisible(x)
}

# Checks ----
#' function to check complex breaks argument
#' @importFrom helpr is_int len_one
#' @noRd
.breaks_check <- function(x) {
  if ( len_one(x) ) {
    if ( is.nan(x) ) {
      return(FALSE)
    }
    if ( is.na(x) ) {
      return(FALSE)
    }
  } else {
    if ( !inherits(x, c("numeric", "factor")) ) {
      return(FALSE)
    }
    if ( any(is.na(x)) || any(is.nan(x)) ) {
      return(FALSE)
    }
  }
  TRUE
}

#' check values of `k`
#' @importFrom helpr is_int
#' @noRd
check_int <- function(x) {
  str <- deparse(substitute(x))
  if ( !is_int(x) || x <= 0L ) {
    stop(sprintf("`%s` must be a positive integer.", str), call. = FALSE)
  }
  invisible(TRUE)
}

# used in factor/character with low num levels
# or numeric with few discrete values
#' @noRd
strat_and_impute <- function(x) {
  if ( any(x_is_na <- is.na(x)) ) {
    signal_info(
      "Imputed stratification for", sum(x_is_na), "missing",
      ifelse(sum(x_is_na) == 1, "value.", "values.")
    )
  }
  imputeNAs(x)
}
