#' Create k-Fold Partitioning
#'
#' Adaptation of the `rsample::vfold_cv()` function and its utilities. Modified
#'   to remove testing and class structures not required for local usage and to
#'   accommodate two-level stratification.
#' Up to 2 levels of stratification can be specified through
#'   the `breaks` parameter:
#'  \itemize{
#'    \item{No stratification: `breaks = NULL`}
#'    \item{One level stratification: `breaks` is a list of length 1, where
#'      the name of the list element specifies the stratification variable and the
#'      value of the element specifies the stratification structure.
#'    }
#'    \item{Two level stratification: `breaks` is a list of length 2, where
#'      the name of the first element specifies the first level stratification
#'      variable and the value of the first element specifies its stratification
#'      structure. Similarly, the name of the second element specifies the
#'      second level stratification variable and its value specifies its
#'      stratification structure.
#'    }
#'  }
#'
#' For stratification variables that are factor, character, or numeric with
#'   5 or fewer unique values, the stratification structure should be set
#'   as `NA`. For example, if stratifying only on status, a binary variable,
#'   `breaks = list(status = NA)`.
#'
#' If the stratification variable is continuous or has more than 5 unique
#'   values, the stratification structure can be specified as either the
#'   number of quantile-based stratification bins or as a numeric vector
#'   providing the bin boundaries (must fully span the range of the
#'   stratification variable). For example, if the stratification variable,
#'   `x`, is a continuous variable in `[0,1]`, `breaks = list(x = 4)` indicates
#'   stratification into 4 bins, the boundaries of which are determined
#'   internally using
#'   ```{r eval = FALSE}
#'   quantile(x, probs = seq(0.0, 1.0, length.out = 5))
#'   ```
#'   and `breaks = list(x = c(0.0, 0.25, 0.75, 1.0))` specifies a 3 bin
#'   structure: `[0, 0.25]`, `(0.25, 0.75]`, and `(0.75, 1.0]`.
#'    **Note:** the lowest boundary is always taken as inclusive.
#'
#' @param data A `data.frame` class object. The data to be subset.
#' @param k `integer(1)`. The number of partitions of the data set.
#' @param repeats `integer(1)`The number of times to repeat the k-fold partitioning.
#' @param breaks A *named* list or `NULL`. If `NULL`, no stratification is performed.
#'    If a named list, must be of length 1 or 2, where the name of the *i^th* element
#'    indicates the column header of `data` containing the *ith* stratification
#'    variable and the value of the *i^th* element specifies the stratification
#'    structure for that variable. See Details for further information.
#' @param ... Variables to be passed to stratification step. Currently limited
#'   to `depth`. The number of stratification bins are based on
#'   `min(5, floor(n / depth))`,  where `n = length(x)`.
#'
#' @return A `x_split` object (extension of the list class). Element `data`
#'   contains the original data. Element `splits` contains a tibble. Each row
#'   of the tibble corresponds to an individual split. Column `split` contains
#'   lists with named elements "analysis" and "assessment". These
#'   elements contain the indices of `data` to be used for each category.
#'   Columns `Fold` and `Repeat` provide fold and repeat indices for
#'   each corresponding split.
#'
#' @examples
#' # no stratification
#' sample_no_strat <- create_kfold(sim_adat, k = 4L, repeats = 2L)
#'
#' # stratification on 1 discrete variable
#' sample_one <- create_kfold(sim_adat, k = 4L, repeats = 2L,
#'                            breaks = list(status = NA))
#'
#' # stratification on 2 variables; 1 continuous + 1 discrete
#' sample_two <- create_kfold(sim_adat, k = 4L, repeats = 2L,
#'                            breaks = list(time = 4L, status = NA))
#'
#' @importFrom globalr is.Integer len_one add_class
#' @export
create_kfold <- function(data, k = 10L, repeats = 1L, breaks = NULL, ...) {

  stopifnot(
    "`data` must be an object of class `data.frame`." =
      !missing(data) && is.data.frame(data),
    "`k` must be a positive integer." = is.Integer(k) && is.vector(k) &&
      len_one(k) && k > 0.0,
    "`repeats` must be a positive integer." = is.Integer(repeats) &&
      is.vector(repeats) && len_one(repeats) && repeats > 0.0,
    # This is not a thorough test of the breaks input; relying on downstream
    # calls to fully vet
    "`breaks` must be NULL or a list of length 1 or 2." =
      is.null(breaks) || (is.list(breaks) && length(breaks) %in% 1:2L)
  )

  # user can currently only pass `depth` through ...
  # potentially expand this to allow for n.unique
  # both are used by the stratification step
  args    <- list(...)
  depth   <- args$depth %||% 20L
  repeats <- as.integer(round(repeats, 0L))

  splits <- replicate(repeats,
    .vfold_splits(data = data, k = k, breaks = breaks, depth = depth),
    simplify = FALSE
  ) |>
    bind_rows(.id = "Repeat") |>
    relocate(-"Repeat")

  if ( repeats == 1L ) {
    splits$Repeat <- NA_integer_
  } else {
    splits$Repeat <- as.integer(splits$Repeat)
  }

  return_obj <- list(data = data, splits = splits)
  .call <- match.call()

  structure(
    add_class(return_obj, "x_split"),
    k = k, repeats = repeats, breaks = breaks, call = .call
  )
}

#' Adaptation of `rsample::vfold_splits()` function to return only indices
#'
#' Function processes stratification variable, calls stratification function,
#'   and post-processes the stratification result into a tibble.
#'
#' @noRd
#' @param data A `data.frame` class object. The data to be subset.
#' @param k Integer. The number of partitions of the data set.
#' @param breaks `NULL` or a list of length 1 or 2. See
#'   description section of `create_kfolds()` for further details.
#' @param depth Integer. Used to determine the best number of bins that
#'   should be used. The number of bins are based on `min(5, floor(n / depth))`
#'   where `n = length(x)`. If x is numeric, there must be at least 40 rows in
#'   the data set (when `depth = 20`) to conduct stratified sampling.
#'
#' @return A tibble. Each row of column `split` contains a two element list
#'   with named elements "analysis" and "assessment". These elements contain the
#'   indices of the data to be used for each category. Column `Fold` provides
#'   the fold designation for each split. And `Repeat` indicates the repeat
#'   number (if applicable).
#' @importFrom tibble tibble
#' @importFrom globalr is.Integer len_one
.vfold_splits <- function(data, k = 10L, breaks = NULL, depth = 20L) {
  # local testing of only those variables used in this function
  # we do not test those that are merely passed to other functions
  stopifnot(
    "`data` must be an object of class `data.frame`." =
      !missing(data) && is.data.frame(data),
    "`k` must be a positive integer." = is.Integer(k) &&
      is.vector(k) && len_one(k) && k > 0.0,
    "`breaks` must be NULL or a list of length 1 or 2." =
      is.null(breaks) || (is.list(breaks) && length(breaks) %in% 1:2L)
  )

  strata <- names(breaks)
  if ( is.list(breaks) && is.null(strata) ) {
    stop("`breaks` must be a named list.", call. = FALSE)
  }

  if ( !is.null(strata) ) {
    # .getStrataIndices expects strata to be a data.frame of values
    # as.data.frame removes possible added classes to base data.frame class
    strata <- tryCatch(as.data.frame(data[, strata, drop = FALSE]),
                       error = function(e) {
                         stop("Unable to retrieve stratification variable(s) ",
                              "from `data`.\n\t", e$message, call. = FALSE)
                       })
  }

  n <- nrow(data)

  indices <- .getStrataIndices(strata, breaks = breaks, k = k, idx = seq_len(n),
                               depth = depth)

  indices <- lapply(indices,
                    function(.ind, .n) {
                      # The sort here is tidier, but warning -- it can mask
                      # unexpected behaviors downstream.
                      list(analysis   = setdiff(seq_len(.n), .ind),
                           assessment = sort(unique(.ind)))
                    }, .n = n)

  tibble(split = indices, Fold = seq_len(k))
}

#' Stratify on a single stratification variable
#'
#' @noRd
#' @param x The vector to be used for stratification.
#' @param breaks Integer, numeric vector, or `NA`. If integer, the number of bins
#'   desired to stratify a numeric stratification variable. If a numeric vector,
#'   the bin boundaries to be used for stratification of a numeric
#'   stratification variable. If `NA`, `x` must be discrete with `n.unique` or
#'   fewer unique cases.
#' @param n.unique Integer. The number of unique value threshold in the algorithm.
#'   At the time of writing, there is no access point to this input. A natural
#'   extension is to allow the user to specify this as an input to `create_kfold()`.
#'   At this time, this value cannot be changed by the user.
#' @param depth Integer. Used to determine the best number of bins that
#'   should be used. The number of bins are based on `min(5, floor(n / depth))`
#'   where `n = length(x)`. If `x` is numeric, there must be at least 40 rows in
#'   the data set (when `depth = 20`) to conduct stratified sampling.
#' @return A factor vector indicating the stratum for each `x`.
#' @importFrom globalr has_length is.Integer len_one value
#' @importFrom stats na.omit quantile
.make_strata <- function(x, breaks = 4, n.unique = 5, depth = 20) {
  stopifnot(
    "All inputs must be provided." = !missing(x),
    "`x` must be a vector." = (is.vector(x) || is.factor(x)) && has_length(x),
    "`breaks` must be an integer or a numeric vector." =
      (is.vector(breaks) && length(breaks) > 1L && is.numeric(breaks) &&
         all(is.finite(breaks) | is.infinite(breaks))) ||
      (is.vector(breaks) && len_one(breaks) &&
         (is.finite(breaks) | (is.na(breaks) && !is.nan(breaks)))),
    "`depth` must be a positive integer." = is.Integer(depth) &&
      is.vector(depth) && len_one(depth) && depth > 0.0,
    "`n.unique` must be a positive integer." = is.Integer(n.unique) &&
      is.vector(n.unique) && len_one(n.unique) && n.unique > 0.0
  )

  num_vals <- unique(na.omit(x))

  if ( length(num_vals) <= n.unique || is.character(x) || is.factor(x) ) {
    # stratification variable is a character, factor or has few unique values,
    # no binning necessary
    # not yet returned as there may be NA values that need to be imputed
    out <- factor(as.character(x))
  } else {
    if ( len_one(breaks) ) {
      # breaks provides the number of bins
      if ( is.na(breaks) ) {
        stop("`x` has ", value(length(num_vals)),
             " unique values; `breaks` cannot be NA.", call. = FALSE)
      }
      n <- length(x)

      # ensure that the stratification will lead to "reasonable" numbers of
      # cases in each bin on average
      if ( floor(n / breaks) < depth ) {
        warning("The number of observations in each quantile is ",
                "below the recommended threshold of ", depth, ".\n",
                "Stratification will be done with ", floor(n / depth),
                " breaks instead.\n",
                "To override this limit, provide `depth` as input.",
                call. = FALSE)
        breaks <- min(breaks, floor(n / depth))
        if ( breaks < 2 ) {
          warning("Too little data to stratify. ",
                  "Non-stratified resampling will be used.", call. = FALSE)
          return(factor(rep("strata1", n)))
        }
      }

      breaks <- quantile(x, probs = seq(0.0, 1.0, length.out = breaks + 1L),
                         na.rm = TRUE)
    }
    breaks <- unique(breaks)
    out <- cut(x, breaks = breaks, include.lowest = TRUE)
  }

  # impute levels for missing values in `x`
  x_is_na <- is.na(x)
  if ( any(x_is_na) ) {
    signal_info("Imputed stratification structure for", sum(x_is_na),
                "missing", ifelse(sum(x_is_na) > 1, "values.", "value."))
    out[x_is_na] <- sample(levels(out), sum(x_is_na), TRUE)
  }

  if ( any(is.na(out)) ) {
    # cut will return NA for values that are outside the range of `breaks`.
    # Require that user provide a more appropriate `breaks` input.
    stop("Provided `breaks` does not span data.\n\t",
         "Range of stratification variable: ", value(range(x, na.rm = TRUE)),
         "\n\tProvided `breaks = ", value(breaks), "`.",
         call. = FALSE
    )
  }
  out
}

#' Stratification step based on 1 stratification variable
#'
#' @noRd
#' @param strata A vector. The values of the stratification variable.
#' @param breaks Integer, numeric vector, or `NA`. If integer, the number of bins
#'   desired to stratify a numeric stratification variable. If a numeric vector,
#'   the bin boundaries to be used for stratification of a numeric
#'   stratification variable. If `NA`, `x` must be discrete with `n.unique` or
#'   fewer unique cases.
#' @param k Integer. The number of partitions of the data set.
#' @param idx Integer vector. The indices of the data to be stratified.
#' @param depth Integer. Used to determine the best number of bins that
#'   should be used. The number of bins are based on `min(5, floor(n / depth))`
#'   where `n = length(x)`. If `x` is numeric, there must be at least 40 rows in
#'   the data set (when `depth = 20`) to conduct stratified sampling.
#'
#' @return A list containing the selected indices for each fold. These are
#'   intended to be the "assessment" sample.
#' @importFrom globalr has_length is.Integer len_one
.getStrataIndices_one <- function(strata, breaks, k, idx, depth, ...) {
  # local testing of only those variables used in this function
  # we do not test those that are merely passed to other functions
  stopifnot(
    "All inputs must be provided." = !missing(strata) &&
      !missing(breaks) && !missing(k) && !missing(idx) && !missing(depth),
    "`k` must be a positive integer." = is.Integer(k) && is.vector(k) &&
      len_one(k) && k > 0.0,
    "`idx` must be an integer vector with dimension matching `strata`." =
      is.Integer(idx) && is.vector(idx) && length(idx) == length(strata) && all(idx > 0)
  )

  stratas <- .make_strata(strata, breaks = breaks, depth = depth)
  # this produces a list of length n_breaks
  stratas <- unname(split(idx, stratas))
  stratas <- lapply(stratas,
                    function(.x, .k) {
                      list(idx   = .x,
                           folds = sample(rep(seq_len(.k), length.out = length(.x))))
                    },
                    .k = k)
  stratas <- bind_rows(stratas)
  unname(split(stratas$idx, stratas$folds))
}

#' Stratification step based on 2 stratification variables
#'
#' @noRd
#' @param strata An object of class `data.frame`. The values of the stratification
#'   variables. Must have 2 columns.
#' @param breaks A list. Each element an integer, numeric vector, or NA.
#' @param k Integer. The number of partitions of the data set.
#' @param idx Integer vector. The indices of the data to be stratified.
#' @param depth Integer. Used to determine the best number of percentiles that
#'   should be used. The number of bins are based on `min(5, floor(n / depth))`
#'   where `n = length(x)`. If `x` is numeric, there must be at least 40 rows in
#'   the data set (when `depth = 20`) to conduct stratified sampling.
#'
#' @return A list containing the selected indices for each fold. These are
#'   intended to be the "assessment" sample.
#' @importFrom purrr transpose
.getStrataIndices_two <- function(strata, breaks, k, idx, depth) {
  # local testing of only those variables used in this function
  # we do not test those that are merely passed to other functions
  stopifnot(
    "All inputs must be provided." = !missing(strata) &&
      !missing(breaks) && !missing(k) && !missing(idx) && !missing(depth),
    "`strata` must be an object of class `data.frame` with 2 columns." =
      is.data.frame(strata) && ncol(strata) == 2L,
    # This breaks test will not catch bad form for all types of list elements
    # However, these will be caught in the calls to the single strata and
    # .make_strata methods
    "`breaks` must be a list of length 2." = is.list(breaks) && length(breaks) == 2L,
    "`k` must be a positive integer." = is.Integer(k) &&
      is.vector(k) && len_one(k) && k > 0.0,
    "`idx` must be an integer vector." = is.Integer(idx) &&
      is.vector(idx) && length(idx) == nrow(strata) && all(idx > 0)
  )

  # stratify on first variable
  stratas_1 <- .make_strata(strata[, 1L], breaks = breaks[[1L]], depth = depth)
  stratas_1 <- unname(split(idx, stratas_1))

  # stratify each element of stratas_1 using second stratification variable
  lapply(stratas_1, function(.i) {
    .getStrataIndices(strata[.i, 2L, drop = FALSE], breaks = breaks[2:2L],
                      k = k, idx = .i, depth = depth)
  }) |>
    transpose() |>        # invert the list
    lapply(base::unlist)  # concatenate within elements (folds)
}

#' Internal function to select appropriate stratification function
#'
#' @noRd
#' @param strata `NULL` or an object of class `data.frame` with 1 or 2 columns.
#'   If not `NULL`, this contains the values of the stratification variable(s).
#' @param k Integer. The number of partitions of the data set.
#' @param idx Integer vector. The indices of the data to be stratified.
#' @param breaks A list. Each element an integer, numeric vector, or `NA`.
#' @param ... Inputs passed on to stratification functions. Must contain `depth`.
#' @return A list, each element providing the indices of the assessment data for
#'   a single fold.
#' @importFrom globalr has_length is.Integer len_one value
.getStrataIndices <- function(strata, k, idx, breaks, ...) {
  stopifnot(
    "All inputs must be provided." = !missing(strata) && !missing(k) &&
      !missing(idx) && !missing(breaks),
    "`k` must be a positive integer." = is.Integer(k) &&
      is.vector(k) && len_one(k) && k > 0.0,
    "`idx` must be an integer vector." = is.Integer(idx) &&
      is.vector(idx) && has_length(idx) && all(idx > 0),
    "`idx` must have same dimension as `strata`" = (!is.null(strata) &&
      (length(idx) == nrow(strata))) || is.null(strata),
    "`breaks` must be `NULL` or a list of length 1 or 2." =
      is.null(breaks) || (is.list(breaks) && length(breaks) %in% 1:2L)
  )

  if ( is.null(strata) ) {
    folds  <- sample(rep(seq_len(k), length.out = length(idx)))
    result <- unname(split(idx, folds))
  } else if ( is.data.frame(strata) ) {
    result <- switch(ncol(strata),
                     .getStrataIndices_one(strata[, 1L], k = k, idx = idx,
                                           breaks = breaks[[1L]], ...),
                     .getStrataIndices_two(strata, k = k, idx = idx,
                                           breaks = breaks, ...),
                     stop("`strata` has inappropriate dimensions. ",
                          "Please contact package manager.", call. = FALSE))
  } else {
    stop("`strata` is of an unexpected class: ", value(class(strata)),
         "Please contact package manager.", call. = FALSE)
  }
  result
}


#' Test for object class "x_split"
#'
#' @rdname create_kfold
#' @param x An `R` object to test.
#' @return [is.x_split()]: Logical. `TRUE` if `x` inherits from
#'   class `x_split`.
#' @export
is.x_split <- function(x) {
  inherits(x, "x_split")
}


#' Retrieve the analysis data from a `x_split` object.
#'
#' [analysis()] : `r lifecycle::badge("questioning")`
#'
#' @rdname create_kfold
#' @param object A `x_split` object.
#' @param i An integer or `NULL`. If an integer, the split for which the
#'   analysis or assessment data is to be retrieved.
#' @return [analysis()]: A list ... each element containing an object of
#'   class `data.frame`.
#' @examples
#' # retrieve analysis data for 2nd split
#' an_2 <- analysis(sample_no_strat, 2L)
#'
#' # retrieve analysis data for all splits
#' an_all <- analysis(sample_no_strat)
#'
#' @importFrom globalr is.Integer len_one
#' @export
analysis <- function(object, i = NULL) {
  stopifnot(
    "`object` must be a `x_split` object." = is.x_split(object),
    "`i` must be a positive integer." = is.null(i) ||
      (is.Integer(i) && is.vector(i) && len_one(i) && i > 0.0),
    "`i` cannot exceed the number of splits." = is.null(i) ||
      (is.numeric(i) && i <= length(object$splits$split))
  )

  if ( is.null(i) ) {
    lapply(object$splits$split, function(.x) object$data[.x$analysis, ])
  } else {
    list(object$data[object$splits$split[[i]]$analysis, ])
  }
}


#' Retrieve the assessment data from a `x_split` object.
#'
#' [assessment()] : `r lifecycle::badge("questioning")`
#'
#' @rdname create_kfold
#' @return [assessment()]: A list ... each element containing an object of
#'   class `data.frame`.
#' @examples
#' # retrieve assessment data for 2nd split
#' ass_2 <- assessment(sample_no_strat, 2L)
#'
#' # retrieve assessment data for all splits
#' ass_all <- assessment(sample_no_strat)
#' @importFrom globalr is.Integer len_one
#' @export
assessment <- function(object, i = NULL) {
  stopifnot(
    "`object` must be a `x_split` object." = is.x_split(object),
    "`i` must be a positive integer." = is.null(i) ||
      (is.Integer(i) && is.vector(i) && len_one(i) && i > 0.0),
    "`i` cannot exceed the number of splits." = is.null(i) ||
      (is.numeric(i) && i <= length(object$splits$split))
  )

  if ( is.null(i) ) {
    lapply(object$splits$split, function(.x) object$data[.x$assessment, ])
  } else {
    list(object$data[object$splits$split[[i]]$assessment, ])
  }
}


#' S3 method for `x_split` object
#' @noRd
#' @importFrom globalr signal_rule value add_color
#' @export
print.x_split <- function(x, ...) {
  writeLines(
    signal_rule("A `x_split` object", lty = "double", line_col = "magenta")
  )
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

  writeLines(signal_rule("split info", line_col = "cyan"))
  print(x$splits)
  writeLines(signal_rule("", lty = "double", line_col = "green"))
  invisible(x)
}
