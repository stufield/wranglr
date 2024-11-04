#' Center and/or Scale Data
#'
#' A function to center and/or scale a data matrix.
#'
#' @param data A `tibble`, `data.frame`, or matrix object with
#'   named data variables to center and/or scale.
#'   If `matrix`, should *only* contain numeric data.
#' @param par_tbl A tibble containing the mean and
#'   standard deviations to use in processing the data.
#'   Must also contain an `feature` column to synchronize
#'   the features with their corresponding scaling parameters.
#'   If `NULL`, a parameter table is generated based on `data`,
#'   i.e. `data` is its own reference.
#' @param feat `character(n)`. A vector indicating which
#'   variables to center/scale.
#' @param center `logical(1)`. Whether the variables
#'   should be shifted to be zero centered (\eqn{\mu = 0}).
#' @param scale `logical(1)`. Whether the variables
#'   should be scaled to have unit variance (\eqn{\sigma = 1}).
#' @return A center/scaled object of the same class as `data`.
#'   Only features specified in `feat` are modified.
#' @author Stu Field
#' @examples
#' scaled <- center_scale(mtcars)
#' apply(strip_meta(scaled), 2, mean) |> sum()  # mean ~ 0
#' apply(strip_meta(scaled), 2, sd)             # sd ~ 1
#'
#' idx <- withr::with_seed(1,
#'   sample(1:nrow(mtcars), size = nrow(mtcars) / 2)
#' )
#' train <- mtcars[idx, ]
#' test  <- mtcars[-idx, ]
#'
#' # Pass `par_tbl` as reference
#' ft <- c("disp", "hp", "drat")
#' par <- tibble::tibble(feature = ft,
#'                       means   = colMeans(strip_meta(train, ft)),
#'                       sds     = apply(strip_meta(train, ft), 2, sd))
#' cs <- center_scale(test, par_tbl = par)
#' @export
center_scale <- function(data, par_tbl = NULL, feat = NULL,
                         center = TRUE, scale = TRUE) {
  UseMethod("center_scale")
}

#' @noRd
#' @export
center_scale.default <- function(data, par_tbl = NULL, feat = NULL,
                                 center = TRUE, scale = TRUE) {
  stop("No S3 method could be found for object of class: ",
       value(class(data)), call. = FALSE)
}

#' @noRd
#' @importFrom purrr pmap
#' @export
center_scale.data.frame <- function(data, par_tbl = NULL, feat = NULL,
                                    center = TRUE, scale = TRUE) {

  if ( is.null(par_tbl) ) {
    par_tbl <- .genParTbl(data, feat)  # generate par_tbl on-the-fly
  } else {
    if ( !.check_par_tbl(par_tbl) ) {
      stop("Please check the `par_tbl` passed to `center_scale()`.",
           call. = FALSE)
    }
    # ensure data and par_tbl are sync'd
    # if user passed, could be out of order
    par_tbl <- rearrange(par_tbl, "feature", names(data))
  }

  feats <- par_tbl$feature
  pars  <- list(data = data[, feats], means = par_tbl$means, sds = par_tbl$sds)

  if ( center && scale ) {
    ret_data <- pmap(pars, function(data, means, sds) (data - means) / sds)
  } else if ( center ) {
    ret_data <- pmap(pars, function(data, means, ...) (data - means))
  } else if ( scale ) {
    ret_data <- pmap(pars, function(data, sds, ...) data / sds)
  } else {
    stop("At least 1 of 'center' or 'scale' must be `TRUE`.", call. = FALSE)
  }
  data[, feats] <- as.data.frame(ret_data) # replace un-scaled -> scaled

  if ( !(center && scale) ) {
    par_tbl$means <- 0
    par_tbl$sds   <- 1
  }

  structure(
    data,
    par_tbl = par_tbl,
    center_lgl = center,
    scale_lgl = scale,
    class = c("cs_trans", class(data))
  )
}

#' @noRd
#' @export
center_scale.tr_data <- center_scale.data.frame

#' @noRd
#' @export
center_scale.soma_adat <- center_scale.data.frame

#' @noRd
#' @export
center_scale.matrix <- function(data, par_tbl = NULL, feat = NULL,
                                center = TRUE, scale = TRUE) {
  stop("The `matrix` method needs work. Please try again later.", call. = FALSE)
  if ( center ) {
    center <- colMeans(data, na.rm = TRUE)
  }
  if ( scale ) {
    scale <- apply(data, 2, stats::sd, na.rm = TRUE)
  }
  scale(data, center = center, scale = scale)
}


#' @describeIn center_scale
#'   tests for presence of `par_tbl` entry in attributes and
#'   if it contains appropriate parameter information that
#'   can be used for centering or scaling data.
#' @examples
#' # Logical test
#' is_center_scaled(cs)
#' @export
is_center_scaled <- function(data) {
  cs  <- inherits(data, "cs_trans")
  tbl <- !is.null(attr(data, "par_tbl"))
  cs && tbl && .check_par_tbl(attr(data, "par_tbl"))
}


#' @describeIn center_scale
#'   the inverse of `center_scale()`. Undo the transformation.
#' @examples
#' # Example of `undo_center_scale()`; reverse above
#' old <- undo_center_scale(cs)
#'
#' # check values are reverted
#' all.equal(test, old)
#'
#' @importFrom stats sd
#' @importFrom purrr pmap
#' @export
undo_center_scale <- function(data, feat = NULL) {
  # do some checking
  # $par_tbl element added in `center_scale()`; required below
  stopifnot(
    "Must perform `undo` on previously center/scaled data." =
      is_center_scaled(data)
  )
  par_tbl <- attr(data, "par_tbl")
  stopifnot(
    "Something wrong with `par_tbl`. Please check `attr(data, \"par_tbl\"')`." =
      .check_par_tbl(par_tbl)
  )
  feats   <- par_tbl$feature
  par_tbl <- rearrange(par_tbl, "feature", names(data)) # sync order
  center  <- attr(data, "center_lgl")
  scale   <- attr(data, "scale_lgl")
  pars    <- as.list(par_tbl)  # convert list
  pars$data <- data[, feats]    # add `data`

  if ( center && scale ) {
    ret_data <- pmap(pars, function(data, means, sds, ...) data * sds + means)
  } else if ( center ) {
    ret_data <- pmap(pars, function(data, means, ...) data + means)
  } else if ( scale ) {
    ret_data <- pmap(pars, function(data, sds, ...) data * sds)
  } else {
    stop(
      "The `par_tbl` attribute is wrong. Check ",
      value("attr(data, \"par_tbl\")"), call. = FALSE
    )
  }

  data[, feats] <- data.frame(ret_data)  # replace un-scaled -> scaled data
  # remove scaling parameters so you cannot 'double' undo
  structure(data, par_tbl = NULL, center_lgl = NULL, scale_lgl = NULL)
}


#' checking function for the parameter table format
#' @noRd
.check_par_tbl <- function(x) {
  # a tibble with feature present
  a <- inherits(x, "tbl_df") && ("feature" %in% names(x))
  # must have at least 1 present
  b <- "means" %in% names(x) || "sds" %in% names(x)
  # if present -> must be double; don't fail if absent; `b` above covers that
  mtype <- ifelse("means" %in% names(x), typeof(x$means) == "double", TRUE)
  stype <- ifelse("sds" %in% names(x), typeof(x$sds) == "double", TRUE)
  (a && b && mtype && stype)
}


#' Generate the parameter table of values for centering and scaling, i.e.
#' means and standard deviations.
#' @importFrom tibble tibble
#' @noRd
.genParTbl <- function(x, feat = NULL) {
  m <- strip_meta(x, feat)
  tibble(
    feature = colnames(m),
    means   = unname(colMeans(m, na.rm = TRUE)),
    sds     = unname(apply(m, 2, stats::sd, na.rm = TRUE))
  )
}
